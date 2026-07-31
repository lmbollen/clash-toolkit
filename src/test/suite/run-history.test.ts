import * as assert from 'assert';
import * as os from 'os';
import * as path from 'path';
import { promises as fs } from 'fs';
import { loadRunModules } from '../../run-loader';
import { listDesignDirs } from '../../run-history-tree';

/**
 * Reading a past run back off disk.
 *
 * The layout these tests build is the one Clash and the runner actually write:
 * Verilog in a directory per component under `02-verilog/`, Yosys output either
 * per module or for the whole design.
 */
suite('Run History', () => {
	let tmpDir: string;
	let counter = 0;

	suiteSetup(async () => {
		tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'clash-run-history-'));
	});

	suiteTeardown(async () => {
		if (tmpDir) { await fs.rm(tmpDir, { recursive: true, force: true }); }
	});

	async function write(file: string, contents = ''): Promise<void> {
		await fs.mkdir(path.dirname(file), { recursive: true });
		await fs.writeFile(file, contents);
	}

	/** A run directory with Clash's per-component Verilog layout. */
	async function runWithVerilog(): Promise<string> {
		const runRoot = path.join(tmpDir, `run${counter++}`);
		await write(path.join(runRoot, '02-verilog', 'Example.Project.topEntity', 'accum.v'));
		await write(path.join(runRoot, '02-verilog', 'ClashSynth_TopEntity.topEntity', 'top_entity.v'));
		return runRoot;
	}

	// The regression: `02-verilog/` holds directories, not files, so listing
	// only its top level found no Verilog for any component — and a row with no
	// Verilog gets no icon to open it with.
	test('a per-module run finds each component\'s own Verilog', async () => {
		const runRoot = await runWithVerilog();
		for (const name of ['top_entity', 'accum']) {
			await write(path.join(runRoot, '03-yosys', 'per-module', name, 'stats.json'), '{}');
		}

		const { modules } = await loadRunModules(runRoot);
		const byName = new Map(modules.map(m => [m.name, m]));

		assert.deepStrictEqual(
			byName.get('accum')?.verilogFiles?.map(f => path.basename(f)),
			['accum.v'],
		);
		assert.deepStrictEqual(
			byName.get('top_entity')?.verilogFiles?.map(f => path.basename(f)),
			['top_entity.v'],
		);
	});

	// Rather than handing it the rest of the design's files.
	test('a component with no Verilog of its own offers none', async () => {
		const runRoot = await runWithVerilog();
		await write(path.join(runRoot, '03-yosys', 'per-module', 'internal_mux', 'stats.json'), '{}');

		const { modules } = await loadRunModules(runRoot);
		assert.deepStrictEqual(modules[0].verilogFiles, []);
	});

	test('a whole-design run offers everything the run generated', async () => {
		const runRoot = await runWithVerilog();
		await write(path.join(runRoot, '03-yosys', 'top_entity.svg'), '<svg/>');

		const { modules } = await loadRunModules(runRoot, { topModule: 'top_entity' } as never);
		assert.deepStrictEqual(
			modules[0].verilogFiles?.map(f => path.basename(f)).sort(),
			['accum.v', 'top_entity.v'],
		);
	});

	// What the History section lists is also what clearing it deletes, so the
	// generated cabal project must not be mistaken for run output.
	suite('which directories are history', () => {
		test('lists design directories, sorted, without the synth project', async () => {
			const clashDir = path.join(tmpDir, `clash${counter++}`);
			for (const name of ['Example.Project.topEntity', 'synth-project', 'Example.Project.accum']) {
				await fs.mkdir(path.join(clashDir, name), { recursive: true });
			}
			await write(path.join(clashDir, 'debug.log'), 'not a directory');

			assert.deepStrictEqual(await listDesignDirs(clashDir), [
				'Example.Project.accum',
				'Example.Project.topEntity',
			]);
		});

		test('a workspace that has never run anything lists nothing', async () => {
			assert.deepStrictEqual(await listDesignDirs(path.join(tmpDir, 'absent')), []);
		});
	});
});
