/**
 * How many things each external tool may do at once.
 *
 * Every tool that can be parallelised gets the same shape of setting: `auto`
 * (the default) derives a count from the machine, and a positive integer
 * overrides it. The tools differ in what a "job" *is* — packages for cabal,
 * modules for GHC, whole components for Yosys, router threads for nextpnr —
 * so each caller supplies its own cap and its own idea of how much work exists.
 */

import * as os from 'os';

/** What a `*Jobs` setting can hold before it is validated. */
export type JobSetting = number | string | null | undefined;

/** Spellings of "work it out from the machine" accepted in settings. */
const AUTO_SPELLINGS = new Set(['auto', 'ncpus', '$ncpus']);

/**
 * Normalise a raw setting value into `'auto'`, a positive integer, or
 * `undefined` when it is unset or nonsense (blank string, zero, 2.5, "many").
 *
 * Nonsense resolves to `undefined` rather than throwing: a bad value in
 * workspace settings should fall back to the default, not fail a build.
 */
export function parseJobSetting(setting: JobSetting): 'auto' | number | undefined {
	if (setting === undefined || setting === null || setting === '') { return undefined; }
	if (typeof setting === 'number') {
		return Number.isInteger(setting) && setting >= 1 ? setting : undefined;
	}
	const value = setting.trim().toLowerCase();
	if (AUTO_SPELLINGS.has(value)) { return 'auto'; }
	if (/^\d+$/.test(value)) {
		const n = Number(value);
		return n >= 1 ? n : undefined;
	}
	return undefined;
}

/**
 * The count `auto` resolves to: one job per core, less one for the extension
 * host and the editor, then capped.
 *
 * The cap is per-tool because the reason to have one is per-tool — for Yosys
 * it is memory (each process holds a whole design), for nextpnr it is that
 * routing stops scaling well before the core count on most boards.
 */
export function autoJobCount(cap: number, cores = os.cpus()?.length ?? 1): number {
	return Math.max(1, Math.min(cap, cores - 1));
}

/**
 * Resolve a setting to a concrete job count.
 *
 * `auto` and unset both derive from the machine and respect `cap`. An explicit
 * number is honoured as given and is **not** capped — a user who asks for 16
 * has said something about their machine that we have no business overriding.
 * `work`, when given, still bounds the result: there is never a reason to run
 * more jobs than there are things to do.
 */
export function resolveJobCount(
	setting: JobSetting,
	opts: { cap: number; work?: number; cores?: number }
): number {
	const parsed = parseJobSetting(setting);
	const wanted = parsed === undefined || parsed === 'auto'
		? autoJobCount(opts.cap, opts.cores)
		: parsed;
	return Math.max(1, opts.work === undefined ? wanted : Math.min(wanted, opts.work));
}
