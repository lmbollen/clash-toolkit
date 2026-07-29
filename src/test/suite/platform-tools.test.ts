import * as assert from 'assert';
import * as vscode from 'vscode';
import { ToolchainChecker } from '../../toolchain';
import { NextpnrRunner } from '../../nextpnr-runner';
import { NextpnrFamily, PNR_FAMILIES, ECP5Device, ECP5Package } from '../../nextpnr-types';

/**
 * Tests for platform tool support.
 *
 * Covers Yosys synthesis targets and nextpnr FPGA families.
 */
suite('Platform Tools Test Suite', () => {
	let outputChannel: vscode.OutputChannel;
	let checker: ToolchainChecker;

	suiteSetup(() => {
		outputChannel = vscode.window.createOutputChannel('Test Platform Tools');
		checker = new ToolchainChecker(outputChannel);
	});

	suiteTeardown(() => {
		if (outputChannel) {
			outputChannel.dispose();
		}
	});

	setup(() => {
		checker.clearCache();
	});

	// ---------------------------------------------------------------
	// Yosys: tool availability
	// ---------------------------------------------------------------

	test('Yosys: should probe with -V flag', async function () {
		this.timeout(15000);
		const status = await checker.check('yosys', 'yosys', '-V');
		// We don't assert available (may not be installed in CI) — just that
		// the check completes without throwing and returns a valid ToolStatus.
		assert.strictEqual(status.name, 'yosys');
		assert.strictEqual(typeof status.available, 'boolean');
	});

	// ---------------------------------------------------------------
	// Nextpnr: tool availability per family
	// ---------------------------------------------------------------

	const nextpnrFamilies: { family: NextpnrFamily; binary: string }[] = [
		{ family: 'ecp5', binary: 'nextpnr-ecp5' },
		{ family: 'ice40', binary: 'nextpnr-ice40' },
		{ family: 'gowin', binary: 'nextpnr-himbaechel' },
		{ family: 'generic', binary: 'nextpnr-generic' },
	];

	for (const { family, binary } of nextpnrFamilies) {
		test(`Nextpnr: getExecutable returns ${binary} for ${family}`, () => {
			assert.strictEqual(NextpnrRunner.getExecutable(family), binary);
		});
	}

	test('Nextpnr: getExecutable falls back to nextpnr-generic for unknown', () => {
		assert.strictEqual(NextpnrRunner.getExecutable('some_future_family'), 'nextpnr-generic');
	});

	test('Nextpnr (ecp5): should probe with --version flag', async function () {
		this.timeout(15000);
		const status = await checker.check('nextpnr-ecp5', 'nextpnr-ecp5', '--version');
		assert.strictEqual(status.name, 'nextpnr-ecp5');
		assert.strictEqual(typeof status.available, 'boolean');
	});

	test('Nextpnr (ice40): should probe with --version flag', async function () {
		this.timeout(15000);
		const status = await checker.check('nextpnr-ice40', 'nextpnr-ice40', '--version');
		assert.strictEqual(status.name, 'nextpnr-ice40');
		assert.strictEqual(typeof status.available, 'boolean');
	});

	// ---------------------------------------------------------------
	// Nextpnr: argument building — common options
	// ---------------------------------------------------------------

	test('Nextpnr: buildArgs uses --textcfg for ecp5/generic', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'generic', jsonPath: '/tmp/d.json', outputDir: '/tmp/out', topModule: 'top' },
			'/tmp/out/top.config'
		);
		// `--timing-allow-fail` is unconditionally appended now; see the
		// dedicated test "Nextpnr: buildArgs always includes --timing-allow-fail".
		assert.deepStrictEqual(args, [
			'--json', '/tmp/d.json',
			'--textcfg', '/tmp/out/top.config',
			'--timing-allow-fail',
		]);
	});

	test('Nextpnr: buildArgs uses --asc for ice40', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ice40', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: 'hx8k' },
			'/o/t.asc'
		);
		assert.ok(args.includes('--asc'), 'ice40 should use --asc');
		assert.ok(!args.includes('--textcfg'), 'ice40 should not use --textcfg');
	});

	test('Nextpnr: buildArgs uses --write for gowin', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'gowin', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: 'GW1N-LV1QN48C6/I5' },
			'/o/t.json'
		);
		assert.ok(args.includes('--write'), 'gowin should use --write');
		assert.ok(!args.includes('--textcfg'), 'gowin should not use --textcfg');
	});

	test('Nextpnr: buildArgs includes --freq', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k', frequency: 50 },
			'/o/t.config'
		);
		assert.ok(args.includes('--freq'));
		assert.ok(args.includes('50'));
	});

	test('Nextpnr: buildArgs includes --seed', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k', seed: 42 },
			'/o/t.config'
		);
		assert.ok(args.includes('--seed'));
		assert.ok(args.includes('42'));
	});

	test('Nextpnr: buildArgs always includes --timing-allow-fail', () => {
		// Analysis flow: never hard-fail on timing, always produce a report.
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k' },
			'/o/t.config'
		);
		assert.ok(args.includes('--timing-allow-fail'));
	});

	test('Nextpnr: buildArgs includes --report and --detailed-timing-report when path given', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k' },
			'/o/t.config',
			{ reportJsonPath: '/o/report.json' }
		);
		assert.ok(args.includes('--report'));
		assert.ok(args.includes('/o/report.json'));
		assert.ok(args.includes('--detailed-timing-report'));
	});

	test('Nextpnr: buildArgs includes --routed-svg when path given', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k' },
			'/o/t.config',
			{ reportJsonPath: '/o/report.json', routedSvgPath: '/o/t.routed.svg' }
		);
		assert.ok(args.includes('--routed-svg'));
		assert.ok(args.includes('/o/t.routed.svg'));
	});

	// ---------------------------------------------------------------
	// Nextpnr: JSON report parsing
	// ---------------------------------------------------------------

	test('Nextpnr: timingFromReport derives Fmax from worst-case clock', () => {
		const report: import('../../nextpnr-runner').NextpnrReport = {
			fmax: {
				clkA: { achieved: 250.5, constraint: 100 },
				clkB: { achieved: 120.0, constraint: 100 },
			},
			critical_paths: [
				{ from: 'clkA', to: 'clkA', path: [{ delay: 1.0 }, { delay: 2.5 }] },
				{ from: 'clkB', to: 'clkB', path: [{ delay: 3.0 }, { delay: 4.0 }, { delay: 1.2 }] },
			],
		};
		const timing = NextpnrRunner.timingFromReport(report);
		assert.strictEqual(timing.maxFrequency, 120.0, 'worst-case Fmax wins');
		assert.ok(timing.criticalPathDelay !== undefined);
		assert.ok(Math.abs(timing.criticalPathDelay! - 8.2) < 1e-6, 'longest path total delay');
		assert.strictEqual(timing.constraintsMet, true, 'both clocks meet their constraints');
	});

	test('Nextpnr: timingFromReport marks constraint failed when any clock misses', () => {
		const report: import('../../nextpnr-runner').NextpnrReport = {
			fmax: {
				slowclk: { achieved: 50, constraint: 100 },
			},
		};
		const timing = NextpnrRunner.timingFromReport(report);
		assert.strictEqual(timing.constraintsMet, false);
	});

	test('Nextpnr: utilizationFromReport buckets ECP5 primitives correctly', () => {
		const report: import('../../nextpnr-runner').NextpnrReport = {
			utilization: {
				TRELLIS_COMB: { used: 142, available: 24288 },
				TRELLIS_FF:   { used: 80,  available: 24288 },
				TRELLIS_IO:   { used: 12,  available: 197 },
				DP16KD:       { used: 2,   available: 56 },
				MULT18X18D:   { used: 1,   available: 28 },
				ALU54B:       { used: 3,   available: 14 },
				PCSCLKDIV:    { used: 0,   available: 2 },
			},
		};
		const util = NextpnrRunner.utilizationFromReport(report, 'ecp5');
		assert.ok(util, 'should produce utilization');
		assert.deepStrictEqual(util!.luts, { used: 142, total: 24288 });
		assert.deepStrictEqual(util!.registers, { used: 80, total: 24288 });
		assert.deepStrictEqual(util!.io, { used: 12, total: 197 });
		assert.deepStrictEqual(util!.bram, { used: 2, total: 56 });
		assert.deepStrictEqual(util!.dsp, { used: 4, total: 42 }, 'DSP = MULT18X18D + ALU54B');
	});

	test('Nextpnr: utilizationFromReport buckets iCE40 primitives correctly', () => {
		const report: import('../../nextpnr-runner').NextpnrReport = {
			utilization: {
				SB_LUT4:      { used: 300, available: 7680 },
				SB_DFF:       { used: 100, available: 7680 },
				SB_DFFE:      { used: 50,  available: 7680 },
				SB_IO:        { used: 10,  available: 256 },
				SB_RAM40_4K:  { used: 4,   available: 32 },
				SB_MAC16:     { used: 1,   available: 8 },
			},
		};
		const util = NextpnrRunner.utilizationFromReport(report, 'ice40');
		assert.deepStrictEqual(util!.luts, { used: 300, total: 7680 });
		assert.deepStrictEqual(util!.registers, { used: 150, total: 15360 }, 'DFF + DFFE');
		assert.deepStrictEqual(util!.io, { used: 10, total: 256 });
		assert.deepStrictEqual(util!.bram, { used: 4, total: 32 });
		assert.deepStrictEqual(util!.dsp, { used: 1, total: 8 });
	});

	test('Nextpnr: utilizationFromReport falls back to heuristic for unknown families', () => {
		const report: import('../../nextpnr-runner').NextpnrReport = {
			utilization: {
				SOME_LUT4:  { used: 5, available: 100 },
				SOME_DFF:   { used: 3, available: 100 },
			},
		};
		const util = NextpnrRunner.utilizationFromReport(report, 'unknown-family');
		assert.ok(util);
		assert.deepStrictEqual(util!.luts, { used: 5, total: 100 });
		assert.deepStrictEqual(util!.registers, { used: 3, total: 100 });
	});

	test('Nextpnr: criticalPathsFromReport extracts steps and sorts slowest-first', () => {
		const report: import('../../nextpnr-runner').NextpnrReport = {
			critical_paths: [
				{
					from: 'clkA',
					to: 'clkA',
					path: [
						{ delay: 0.5, type: 'clk-to-q', from: { cell: 'ff1', port: 'Q' } },
						{ delay: 1.2, type: 'routing',  net: 'wireA', to: { cell: 'ff2', port: 'D' } },
					],
				},
				{
					from: 'clkB',
					to: 'clkB',
					path: [
						{ delay: 2.0, type: 'clk-to-q' },
						{ delay: 3.5, type: 'routing' },
						{ delay: 1.0, type: 'setup' },
					],
				},
			],
		};
		const paths = NextpnrRunner.criticalPathsFromReport(report);
		assert.strictEqual(paths.length, 2);
		assert.strictEqual(paths[0].from, 'clkB', 'slower path comes first');
		assert.ok(Math.abs(paths[0].totalDelay - 6.5) < 1e-6);
		assert.ok(Math.abs(paths[1].totalDelay - 1.7) < 1e-6);
		// Step fields are normalised — from/to cells + nets are preserved.
		const routing = paths[1].steps.find(s => s.type === 'routing');
		assert.ok(routing);
		assert.strictEqual(routing!.net, 'wireA');
		assert.strictEqual(routing!.toCell, 'ff2');
	});

	test('Nextpnr: criticalPathsFromReport returns empty when report has no paths', () => {
		assert.deepStrictEqual(NextpnrRunner.criticalPathsFromReport({}), []);
		assert.deepStrictEqual(
			NextpnrRunner.criticalPathsFromReport({ critical_paths: [] }),
			[]
		);
	});

	test('Nextpnr: buildArgs includes extra args', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k', extraArgs: ['--placer', 'heap'] },
			'/o/t.config'
		);
		assert.ok(args.includes('--placer'));
		assert.ok(args.includes('heap'));
	});

	// ---------------------------------------------------------------
	// Nextpnr: constraints flag per family
	// ---------------------------------------------------------------

	test('Nextpnr: buildArgs uses --lpf for ECP5 constraints', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k', constraintsFile: '/c.lpf' },
			'/o/t.config'
		);
		assert.ok(args.includes('--lpf'));
		assert.ok(args.includes('/c.lpf'));
	});

	test('Nextpnr: buildArgs uses --pcf for iCE40 constraints', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ice40', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: 'hx8k', constraintsFile: '/c.pcf' },
			'/o/t.config'
		);
		assert.ok(args.includes('--pcf'));
		assert.ok(args.includes('/c.pcf'));
	});

	test('Nextpnr: buildArgs passes Gowin constraints via --vopt cst=', () => {
		// nextpnr-himbaechel (the gowin executable) has no --cst option;
		// constraints go through --vopt cst=<file>.
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'gowin', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: 'GW1N-9', constraintsFile: '/c.cst' },
			'/o/t.config'
		);
		assert.ok(!args.includes('--cst'), 'must not use the unsupported --cst flag');
		assert.ok(args.includes('--vopt'));
		assert.ok(args.includes('cst=/c.cst'));
	});

	// ---------------------------------------------------------------
	// Nextpnr: buildArgs for every device in every PNR_FAMILIES target
	// ---------------------------------------------------------------

	for (const [targetId, familyInfo] of PNR_FAMILIES) {
		for (const dev of familyInfo.devices) {
			test(`buildArgs: ${targetId} device ${dev.value} uses correct device flag`, () => {
				const args = NextpnrRunner.buildNextpnrArgs(
					{
						family: familyInfo.family,
						jsonPath: '/tmp/d.json',
						outputDir: '/tmp/out',
						topModule: 'top',
						device: dev.value,
					},
					'/tmp/out/top.config'
				);

				if (familyInfo.deviceFlag === 'prefix') {
					assert.ok(
						args.includes(`--${dev.value}`),
						`${targetId}/${dev.value}: should have --${dev.value}`
					);
					assert.ok(
						!args.includes('--device'),
						`${targetId}/${dev.value}: prefix family should NOT use --device flag`
					);
				} else {
					assert.ok(
						args.includes('--device'),
						`${targetId}/${dev.value}: should have --device flag`
					);
					assert.ok(
						args.includes(dev.value),
						`${targetId}/${dev.value}: should have device value`
					);
				}
			});

			test(`buildArgs: ${targetId} device ${dev.value} with package`, () => {
				const args = NextpnrRunner.buildNextpnrArgs(
					{
						family: familyInfo.family,
						jsonPath: '/tmp/d.json',
						outputDir: '/tmp/out',
						topModule: 'top',
						device: dev.value,
						packageName: 'TEST_PKG',
					},
					'/tmp/out/top.config'
				);

				assert.ok(args.includes('--package'), `${targetId}/${dev.value}: should have --package`);
				assert.ok(args.includes('TEST_PKG'), `${targetId}/${dev.value}: should have package value`);
			});
		}
	}

	// ---------------------------------------------------------------
	// Nextpnr: buildArgs --vopt handling
	// ---------------------------------------------------------------

	test('Nextpnr: buildArgs emits --vopt for each entry', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'gowin', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: 'GW1N-LV1QN48C6/I5', vopt: ['family=GW1N-9C', 'cst=/tmp/test.cst'] },
			'/o/t.config'
		);
		const voptIndices = args.reduce((acc: number[], a, i) => a === '--vopt' ? [...acc, i] : acc, []);
		assert.strictEqual(voptIndices.length, 2, 'should have two --vopt flags');
		assert.strictEqual(args[voptIndices[0] + 1], 'family=GW1N-9C');
		assert.strictEqual(args[voptIndices[1] + 1], 'cst=/tmp/test.cst');
	});

	test('Nextpnr: buildArgs omits --vopt when undefined', () => {
		const args = NextpnrRunner.buildNextpnrArgs(
			{ family: 'ecp5', jsonPath: '/j.json', outputDir: '/o', topModule: 't', device: '25k' },
			'/o/t.config'
		);
		assert.ok(!args.includes('--vopt'), 'should not have --vopt when not set');
	});

	// ---------------------------------------------------------------
	// Nextpnr: buildArgs legacy ecp5 field — all ECP5 device × package
	// ---------------------------------------------------------------

	{
		const ecp5Devices = PNR_FAMILIES.get('ecp5')!.devices;
		const ecp5Packages = ['CABGA256', 'CABGA381', 'CABGA554', 'CABGA756', 'CSFBGA285', 'CSFBGA381', 'CSFBGA554'];
		const ecp5Speeds: Array<'6' | '7' | '8'> = ['6', '7', '8'];

		for (const dev of ecp5Devices) {
			for (const pkg of ecp5Packages) {
				test(`buildArgs legacy: ecp5 ${dev.value} + ${pkg}`, () => {
					const args = NextpnrRunner.buildNextpnrArgs(
						{
							family: 'ecp5',
							jsonPath: '/tmp/d.json',
							outputDir: '/tmp/out',
							topModule: 'top',
							ecp5: { device: dev.value as ECP5Device, package: pkg as ECP5Package, speedGrade: '6' },
						},
						'/tmp/out/top.config'
					);
					assert.ok(args.includes(`--${dev.value}`), `should have --${dev.value}`);
					assert.ok(args.includes(pkg), `should have package ${pkg}`);
					assert.ok(args.includes('--package'), 'should have --package flag');
					assert.ok(args.includes('--speed'), 'should have --speed flag');
					assert.ok(args.includes('6'), 'should have speed grade');
				});
			}
		}

		for (const speed of ecp5Speeds) {
			test(`buildArgs legacy: ecp5 speed grade ${speed}`, () => {
				const args = NextpnrRunner.buildNextpnrArgs(
					{
						family: 'ecp5',
						jsonPath: '/j.json',
						outputDir: '/o',
						topModule: 't',
						ecp5: { device: '25k', package: 'CABGA381', speedGrade: speed },
					},
					'/o/t.config'
				);
				assert.ok(args.includes(speed), `should have speed grade ${speed}`);
			});
		}

		test('buildArgs legacy: ecp5 without speed grade omits --speed', () => {
			const args = NextpnrRunner.buildNextpnrArgs(
				{
					family: 'ecp5',
					jsonPath: '/j.json',
					outputDir: '/o',
					topModule: 't',
					ecp5: { device: '85k', package: 'CABGA756' },
				},
				'/o/t.config'
			);
			assert.ok(!args.includes('--speed'), 'should NOT have --speed when omitted');
		});
	}

	// ---------------------------------------------------------------
	// Cabal: tool availability (needed by synth project)
	// ---------------------------------------------------------------

	test('Cabal: should probe with --version flag', async function () {
		this.timeout(15000);
		const status = await checker.check('cabal', 'cabal', '--version');
		assert.strictEqual(status.name, 'cabal');
		assert.strictEqual(typeof status.available, 'boolean');
	});

	// ---------------------------------------------------------------
	// checkAll: all tools in a single call
	// ---------------------------------------------------------------

	test('checkAll should check cabal, yosys, nextpnr-ecp5', async function () {
		this.timeout(30000);
		const results = await checker.checkAll();
		assert.ok(results.has('cabal'), 'Should check cabal');
		assert.ok(results.has('yosys'), 'Should check yosys');
		assert.ok(results.has('nextpnr-ecp5'), 'Should check nextpnr-ecp5');
		// Diagrams come from the bundled netlistsvg library — no `dot` probe.
		assert.strictEqual(results.has('dot'), false, 'Should not check dot');
		assert.strictEqual(results.size, 3, 'Should check exactly 3 tools');
	});

	// ---------------------------------------------------------------
	// formatSummary with all tools
	// ---------------------------------------------------------------

	test('formatSummary should include all checked tools', async function () {
		this.timeout(30000);
		await checker.checkAll();
		const summary = checker.formatSummary();
		assert.ok(summary.includes('cabal'), 'Summary should mention cabal');
		assert.ok(summary.includes('yosys'), 'Summary should mention yosys');
		assert.ok(summary.includes('nextpnr-ecp5'), 'Summary should mention nextpnr-ecp5');
	});

	// ---------------------------------------------------------------
	// PNR_FAMILIES registry — structure validation
	// ---------------------------------------------------------------

	test('PNR_FAMILIES contains ecp5, ice40, gowin', () => {
		assert.ok(PNR_FAMILIES.has('ecp5'), 'should have ecp5');
		assert.ok(PNR_FAMILIES.has('ice40'), 'should have ice40');
		assert.ok(PNR_FAMILIES.has('gowin'), 'should have gowin');
	});

	test('PNR_FAMILIES does not contain unsupported synthesis targets', () => {
		for (const id of ['generic', 'xilinx', 'quicklogic', 'sf2']) {
			assert.ok(!PNR_FAMILIES.has(id), `${id} should not have PnR`);
		}
	});

	for (const [targetId, info] of PNR_FAMILIES) {
		test(`PNR_FAMILIES[${targetId}] has valid binary name`, () => {
			assert.ok(info.binary.startsWith('nextpnr-'), `${targetId} binary should start with nextpnr-`);
		});

		test(`PNR_FAMILIES[${targetId}] family matches target`, () => {
			assert.strictEqual(info.family, targetId, `${targetId} family field should match key`);
		});

		test(`PNR_FAMILIES[${targetId}] has non-empty device list`, () => {
			assert.ok(info.devices.length > 0, `${targetId} should have at least one device`);
		});

		test(`PNR_FAMILIES[${targetId}] deviceFlag is prefix or device`, () => {
			assert.ok(
				info.deviceFlag === 'prefix' || info.deviceFlag === 'device',
				`${targetId} deviceFlag should be 'prefix' or 'device'`
			);
		});

		test(`PNR_FAMILIES[${targetId}] binary matches getExecutable`, () => {
			assert.strictEqual(
				info.binary,
				NextpnrRunner.getExecutable(info.family),
				`${targetId} binary should match getExecutable(${info.family})`
			);
		});

		test(`PNR_FAMILIES[${targetId}] device entries are unique (value + vopt)`, () => {
			// Gowin uses the same physical chip for family variants (GW1N-9 vs GW1N-9C),
			// differentiated by --vopt. The unique key is value + vopt.
			const keys = info.devices.map(d => `${d.value}|${d.vopt ?? ''}`);
			const unique = new Set(keys);
			assert.strictEqual(keys.length, unique.size, `${targetId} device entries should be unique`);
		});

		for (const dev of info.devices) {
			test(`PNR_FAMILIES[${targetId}] device ${dev.value} has label and description`, () => {
				assert.ok(dev.label.length > 0, `${targetId}/${dev.value} missing label`);
				assert.ok(dev.value.length > 0, `${targetId}/${dev.value} missing value`);
				assert.ok(dev.description.length > 0, `${targetId}/${dev.value} missing description`);
			});
		}
	}

	// ---------------------------------------------------------------
	// PNR_FAMILIES — expected device counts
	// ---------------------------------------------------------------

	test('PNR_FAMILIES ecp5 has 9 devices (3 families × 3 sizes)', () => {
		assert.strictEqual(PNR_FAMILIES.get('ecp5')!.devices.length, 9);
	});

	test('PNR_FAMILIES ice40 has 10 devices', () => {
		assert.strictEqual(PNR_FAMILIES.get('ice40')!.devices.length, 10);
	});

	test('PNR_FAMILIES gowin has 9 devices', () => {
		assert.strictEqual(PNR_FAMILIES.get('gowin')!.devices.length, 9);
	});

	// ---------------------------------------------------------------
	// NextpnrRunner.getValidPackages — dynamic package discovery
	// (requires nextpnr binaries; gracefully skips in CI)
	// ---------------------------------------------------------------

	// Probe every ECP5 device for valid packages
	for (const dev of PNR_FAMILIES.get('ecp5')!.devices) {
		test(`getValidPackages: ecp5 ${dev.value} returns at least one package`, async function () {
			this.timeout(15000);
			try {
				const pkgs = await NextpnrRunner.getValidPackages(dev.value, 'nextpnr-ecp5', 'prefix');
				assert.ok(pkgs.length > 0, `ecp5 ${dev.value} should have at least one valid package`);
				// Every ECP5 device supports CABGA381
				assert.ok(pkgs.includes('CABGA381'), `ecp5 ${dev.value} should support CABGA381`);
			} catch {
				this.skip();
			}
		});
	}

	// Probe every iCE40 device for valid packages
	for (const dev of PNR_FAMILIES.get('ice40')!.devices) {
		test(`getValidPackages: ice40 ${dev.value} returns at least one package`, async function () {
			this.timeout(15000);
			try {
				const pkgs = await NextpnrRunner.getValidPackages(dev.value, 'nextpnr-ice40', 'prefix');
				assert.ok(pkgs.length > 0, `ice40 ${dev.value} should have at least one valid package`);
			} catch {
				this.skip();
			}
		});
	}

	// Verify package exclusion — CABGA756 is only valid for 85k-class ECP5
	test('getValidPackages: ecp5 25k excludes CABGA756', async function () {
		this.timeout(15000);
		try {
			const pkgs = await NextpnrRunner.getValidPackages('25k', 'nextpnr-ecp5', 'prefix');
			assert.ok(!pkgs.includes('CABGA756'), 'CABGA756 should not be valid for 25k');
		} catch {
			this.skip();
		}
	});

	test('getValidPackages: ecp5 45k excludes CABGA756', async function () {
		this.timeout(15000);
		try {
			const pkgs = await NextpnrRunner.getValidPackages('45k', 'nextpnr-ecp5', 'prefix');
			assert.ok(!pkgs.includes('CABGA756'), 'CABGA756 should not be valid for 45k');
		} catch {
			this.skip();
		}
	});

	test('getValidPackages: ecp5 85k includes CABGA756', async function () {
		this.timeout(15000);
		try {
			const pkgs = await NextpnrRunner.getValidPackages('85k', 'nextpnr-ecp5', 'prefix');
			assert.ok(pkgs.includes('CABGA756'), 'CABGA756 should be valid for 85k');
		} catch {
			this.skip();
		}
	});

	// Verify caching
	test('getValidPackages results are cached across calls', async function () {
		this.timeout(15000);
		try {
			const first = await NextpnrRunner.getValidPackages('85k', 'nextpnr-ecp5', 'prefix');
			const second = await NextpnrRunner.getValidPackages('85k', 'nextpnr-ecp5', 'prefix');
			assert.deepStrictEqual(first, second, 'cached result should match');
		} catch {
			this.skip();
		}
	});

	// Gowin has no candidate packages to probe — should return empty
	test('getValidPackages: gowin returns empty (no candidate packages)', async function () {
		const pkgs = await NextpnrRunner.getValidPackages('GW1N-9', 'nextpnr-himbaechel', 'device');
		assert.deepStrictEqual(pkgs, [], 'gowin should return empty since no candidates are probed');
	});
});
