# Releasing

Publishing a version of the extension to the VS Code Marketplace. Every step is
a command you can run locally; nothing here is automated by CI, which builds and
lints each push but never publishes.

## Before you start

- A clean working tree apart from the changes going into the release.
- `nix develop` (or `direnv allow`), which provides node, `vsce`, `mdbook`, and
  the EDA tools the integration tests need.
- A **Personal Access Token** for the `LucasBollen` publisher, from Azure DevOps
  with the *Marketplace → Manage* scope. Either export it as `VSCE_PAT` or run
  `npx vsce login LucasBollen` once. Publishing is the only step that needs it.

## 1. Write the changelog entry

`CHANGELOG.md` follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Rename the `## [Unreleased]` heading to the new version and today's date, and
sort what is under it into **Added / Changed / Deprecated / Removed / Fixed**, in
that order.

Two things worth the effort, because this file is what users read in the
Marketplace's *Changelog* tab:

- **Say why, not just what.** A behaviour that changed is easier to accept when
  the entry says what was wrong with the old one.
- **Give removals their own section.** A setting that disappeared, or a renamed
  view id, is the entry someone searches for when their configuration stops
  working. Say what happens to a value left behind in `settings.json`.

## 2. Pick and set the version

`npm version <x.y.z> --no-git-tag-version` — this updates `package.json` and both
version fields in `package-lock.json`, and the `--no-git-tag-version` keeps it
from tagging before the work is committed.

The extension is pre-1.0, so:

| Change | Bump |
|---|---|
| Bug fixes only | patch — `0.3.0` → `0.3.1` |
| New features, removed settings, renamed contribution points | minor — `0.3.1` → `0.4.0` |

A published version number can never be reused, so it is worth getting right
before the publish step rather than after.

## 3. Run the test suite

```bash
npm run clean && bash scripts/test.sh
```

`scripts/test.sh` compiles, lints, then runs the suite in a real VS Code
instance. It must be a **clean** build: `out/` keeps the compiled JavaScript of
deleted test files, and mocha runs whatever it finds there, so a stale artifact
fails a suite that no longer exists. See [Testing](testing.md) for what the
suites cover.

## 4. Check the documentation still describes the extension

The book is the documentation, so a release with stale pages ships stale docs.

```bash
mdbook build book        # must succeed
```

A build only proves the Markdown parses. What it cannot check, and you should:

- **Settings**: every property in `contributes.configuration` appears in
  `book/src/guide/configuration.md`, and nothing removed still does.
- **Commands**: `contributes.commands` against `book/src/guide/commands.md`.
- **Contribution points**: view ids, context values and menu entries against
  `book/src/guide/sidebar.md`.
- **Test suites**: `src/test/suite/*.test.ts` against the table in
  `book/src/dev/testing.md`.

A quick way to catch the first one:

```bash
node -e "
const cfg = require('./package.json').contributes.configuration;
const doc = require('fs').readFileSync('book/src/guide/configuration.md','utf8');
for (const cat of cfg)
  for (const key of Object.keys(cat.properties))
    if (!doc.includes('\`' + key.replace('clash-toolkit.','') + '\`'))
      console.log('undocumented:', key);
"
```

Grepping the book for the name of anything you deleted is the other half — a
removed setting or view id usually appears in more pages than you remember.

## 5. Verify what will be packaged

```bash
npm run verify:package
```

`scripts/verify-package.js` runs `vsce ls` and checks the real file list against
a deny list and a required list. It exists because 0.3.0 shipped
`.clash/debug.log`, with absolute paths from the machine that built it: `vsce`
reads only `.vscodeignore`, never `.gitignore`, so being untracked is not enough
to keep a file out of the package.

It runs automatically from `vscode:prepublish`, which gates both `vsce package`
and `vsce publish` — but running it directly gives a readable report.

Also confirm `package.json` still describes the extension: `displayName`,
`description`, `categories`, `keywords`, `icon`, `repository`, `license`, and
the `engines.vscode` range you actually test against.

## 6. Commit

Conventional commits, as the history uses:

```
feat(sidebar): fold the three views into one
fix(history): resolve each component's Verilog
chore(release): 0.4.0
```

Keep the version bump and changelog as their own `chore(release)` commit at the
end, so the release is one commit to tag and revert.

## 7. Build the package

```bash
npx vsce package
```

Produces `clash-toolkit-<version>.vsix`. Install it into a real editor before
publishing — the test suite drives the extension host, but it does not look at
the result:

```bash
code --install-extension clash-toolkit-<version>.vsix
```

Then open a Clash project and exercise what changed: the sidebar, a synthesis
run, and any command the release touched.

## 8. Publish

```bash
npx vsce publish          # or: npx vsce publish --packagePath clash-toolkit-<version>.vsix
```

**This is public and cannot be undone.** A version can be unpublished, but its
number can never be reused, and anyone who has already installed it keeps it.
Publish only from a commit that is in the repository, so what is on the
Marketplace can always be traced back to a tree.

Afterwards:

```bash
git tag -a v<version> -m "v<version>"
git push && git push --tags
```

Pushing to `master` also republishes the book, which the docs workflow builds
from `book/` on every push.
