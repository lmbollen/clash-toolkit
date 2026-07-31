import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { promises as fs } from 'fs';
import { ClashManifestParser, pnrTargetClock } from '../../clash-manifest-parser';

/**
 * What place & route is constrained against comes from the manifest, and the
 * manifest states all of it: which ports are clocks, which domain each is in,
 * and every domain's period.  So these tests are about refusing to fill gaps —
 * a wrong frequency produces a confident "constraints met" verdict about a
 * constraint the design never had, which is worse than not running.
 */
suite('Manifest Clocks', () => {
	let parser: ClashManifestParser;
	let tmpDir: string;

	suiteSetup(async () => {
		parser = new ClashManifestParser();
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-manifest-clocks-'));
	});

	suiteTeardown(async () => {
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	let counter = 0;

	/**
	 * Write a minimal manifest and parse it back.  Domains arrive as
	 * `[name, periodPs]` pairs and clock ports as `[port, domain]`: Clash domain
	 * names are PascalCase, which is not a shape the lint rules allow as an
	 * object-literal key.
	 */
	async function parse(
		domains: [string, unknown][],
		clockPorts: [string, string | undefined][] = [],
	) {
		const dir = path.join(tmpDir, `m${counter++}`);
		await fs.mkdir(dir, { recursive: true });
		const manifest = {
			files: [{ name: 'top_entity.v' }],
			components: [],
			domains: Object.fromEntries(domains.map(([name, period]) => [name, { period }])),
			top_component: {
				name: 'top_entity',
				ports_flat: [
					...clockPorts.map(([name, domain]) => ({ name, is_clock: true, domain })),
					{ name: 'result', is_clock: false },
				],
			},
		};
		await fs.writeFile(
			path.join(dir, 'clash-manifest.json'),
			JSON.stringify(manifest),
		);
		return parser.parseManifest(path.join(dir, 'clash-manifest.json'));
	}

	// The regression behind all of this: a manifest lists every domain the
	// design mentions, so preferring "System" by name reported 100 MHz for a
	// design whose top entity is clocked at 50.
	test('a clock takes the domain its port declares, never one picked by name', async () => {
		const m = await parse(
			[['System', 10_000], ['Dom50', 20_000]],
			[['clk', 'Dom50']],
		);
		assert.deepStrictEqual(m.topClocks, [
			{ port: 'clk', domain: 'Dom50', periodPs: 20_000, frequencyMHz: 50 },
		]);
		assert.strictEqual(pnrTargetClock(m)?.frequencyMHz, 50);
	});

	test('a domain period in picoseconds becomes a frequency in MHz', async () => {
		const m = await parse([['Dom100', 10_000]], [['clk', 'Dom100']]);
		assert.strictEqual(m.topClocks[0].frequencyMHz, 100);
	});

	// Not a missing value to be filled in: a combinational top entity genuinely
	// has no frequency, and nextpnr reports an unconstrained Fmax for it.
	test('a top entity with no clock port has no clocks and no target', async () => {
		const m = await parse([['System', 10_000]]);
		assert.deepStrictEqual(m.topClocks, []);
		assert.strictEqual(pnrTargetClock(m), undefined);
	});

	test('a clock naming a domain the manifest does not define is an error', async () => {
		await assert.rejects(
			() => parse([['System', 10_000]], [['clk', 'Dom50']]),
			/domain "Dom50", which the manifest does not define \(it defines: System\)/,
		);
	});

	test('a clock port with no domain is an error', async () => {
		await assert.rejects(
			() => parse([['System', 10_000]], [['clk', undefined]]),
			/clock port "clk" declares no clock domain/,
		);
	});

	test('a domain without a usable period is an error', async () => {
		await assert.rejects(
			() => parse([['Broken', 0]], [['clk', 'Broken']]),
			/domain "Broken" has no usable clock period/,
		);
		await assert.rejects(
			() => parse([['Broken', null]], [['clk', 'Broken']]),
			/domain "Broken" has no usable clock period \(got null\)/,
		);
	});

	// One `--freq` covers the whole design, so two domains cannot both be met.
	test('a design clocked by two domains has no single target and says so', async () => {
		const m = await parse(
			[['Dom50', 20_000], ['Dom100', 10_000]],
			[['clk_slow', 'Dom50'], ['clk_fast', 'Dom100']],
		);
		assert.strictEqual(m.topClocks.length, 2);
		assert.throws(
			() => pnrTargetClock(m),
			/driven by clocks in 2 domains.*clk_slow \(Dom50, 50\.00 MHz\).*clk_fast \(Dom100, 100\.00 MHz\)/s,
		);
	});

	test('several clock ports in one domain are one constraint', async () => {
		const m = await parse(
			[['Dom50', 20_000]],
			[['clk_a', 'Dom50'], ['clk_b', 'Dom50']],
		);
		assert.strictEqual(pnrTargetClock(m)?.frequencyMHz, 50);
	});

	test('a run with no manifest cannot be constrained', () => {
		assert.throws(() => pnrTargetClock(undefined), /No Clash manifest/);
	});
});
