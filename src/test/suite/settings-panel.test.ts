import * as assert from 'assert';

import { customScriptKey, SynthesisSettingsPanel } from '../../synthesis-settings-panel';

/**
 * The settings panel's webview HTML is one big string the extension host never
 * executes, so a syntax error in its embedded <script> is invisible to the
 * compiler and to every host-side test — the panel just dies on open. Parse it
 * the way the webview would.
 */
suite('Synthesis Settings Panel', () => {

	test('the embedded webview script is syntactically valid JavaScript', () => {
		// buildHtml is private by design; reach past the modifier rather than
		// widening the API for a test.
		const html = (SynthesisSettingsPanel as unknown as {
			buildHtml(): string;
		}).buildHtml();

		const open = html.lastIndexOf('<script>');
		const close = html.lastIndexOf('</script>');
		assert.ok(open !== -1 && close > open, 'HTML should embed a script block');

		const body = html.slice(open + '<script>'.length, close);
		// new Function parses without executing — acquireVsCodeApi etc. are
		// never called, so this is safe in the extension host.
		assert.doesNotThrow(
			() => new Function(body),
			'webview script must parse; a syntax error here kills the whole panel'
		);
	});

	test('script overrides are stored per mode, and per target only for targets', () => {
		assert.strictEqual(customScriptKey('ooc', 'ecp5'), 'outOfContextScript');
		assert.strictEqual(customScriptKey('ooc', 'ice40'), 'outOfContextScript');
		assert.strictEqual(customScriptKey('target', 'ecp5'), 'synthesisScript.ecp5');
	});
});
