import * as assert from 'assert';

import { parseJobSetting, autoJobCount, resolveJobCount } from '../../parallelism';
import { resolveCabalJobs } from '../../clash-compiler';
import { perModuleConcurrency, YOSYS_AUTO_JOB_CAP } from '../../yosys-runner';

/**
 * Every parallelisable tool takes the same shape of setting: `auto` derives a
 * count from the machine, a positive integer overrides it. What differs per
 * tool is the cap on `auto` and whether there is a bounded amount of work.
 */
suite('Job settings', () => {

	suite('parsing', () => {
		test('auto is accepted in the spellings settings might carry', () => {
			for (const v of ['auto', 'AUTO', ' auto ', 'ncpus', '$ncpus']) {
				assert.strictEqual(parseJobSetting(v), 'auto', `"${v}" should mean auto`);
			}
		});

		test('positive integers pass through, as numbers or strings', () => {
			assert.strictEqual(parseJobSetting(4), 4);
			assert.strictEqual(parseJobSetting('4'), 4);
			assert.strictEqual(parseJobSetting(1), 1);
		});

		test('unset and nonsense both fall back rather than throwing', () => {
			// A bad value in workspace settings must not fail a build.
			for (const v of [undefined, null, '', 0, -1, 2.5, 'many', '3x']) {
				assert.strictEqual(
					parseJobSetting(v as never), undefined,
					`${JSON.stringify(v)} should not resolve to a count`
				);
			}
		});
	});

	suite('auto', () => {
		test('leaves a core for the editor and respects the cap', () => {
			assert.strictEqual(autoJobCount(8, 16), 8, 'capped at 8 on a 16-core box');
			assert.strictEqual(autoJobCount(8, 5), 4, '5 cores, one left for the editor');
			assert.strictEqual(autoJobCount(4, 16), 4, 'a lower cap wins');
		});

		test('never drops below one, however few cores are reported', () => {
			assert.strictEqual(autoJobCount(8, 1), 1);
			assert.strictEqual(autoJobCount(8, 0), 1);
		});
	});

	suite('resolution', () => {
		test('an explicit count is honoured above the auto cap', () => {
			// The cap exists to pick a safe default, not to overrule someone who
			// has said something about their own machine.
			assert.strictEqual(resolveJobCount(32, { cap: 4, cores: 8 }), 32);
		});

		test('work bounds the result — no more jobs than there are things to do', () => {
			assert.strictEqual(resolveJobCount(32, { cap: 8, work: 3, cores: 16 }), 3);
			assert.strictEqual(resolveJobCount('auto', { cap: 8, work: 2, cores: 16 }), 2);
		});

		test('zero work still resolves to one job, not zero', () => {
			// A pool of zero workers would never drain its queue.
			assert.strictEqual(resolveJobCount('auto', { cap: 8, work: 0, cores: 16 }), 1);
		});

		test('nonsense resolves as auto', () => {
			assert.strictEqual(
				resolveJobCount('many', { cap: 8, cores: 16 }),
				autoJobCount(8, 16)
			);
		});
	});

	suite('per tool', () => {
		test('cabal keeps its own spelling of auto rather than a number', () => {
			// cabal works the count out itself; resolving it here would freeze a
			// number into a flag cabal is happy to compute.
			assert.strictEqual(resolveCabalJobs('auto'), '$ncpus');
			assert.strictEqual(resolveCabalJobs(4), '4');
			assert.strictEqual(resolveCabalJobs('nonsense'), undefined);
		});

		test('yosys is capped on auto and bounded by the component count', () => {
			assert.ok(perModuleConcurrency(100) <= YOSYS_AUTO_JOB_CAP);
			assert.strictEqual(perModuleConcurrency(2), Math.min(2, perModuleConcurrency(100)));
			assert.strictEqual(perModuleConcurrency(100, 1), 1, 'explicit 1 runs sequentially');
			assert.strictEqual(perModuleConcurrency(100, 16), 16, 'explicit counts are not capped');
			assert.strictEqual(perModuleConcurrency(3, 16), 3, 'but never exceed the work');
		});
	});
});
