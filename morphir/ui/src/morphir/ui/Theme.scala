package morphir.ui

/**
 * The morphir client theme: a dark, T3-code-style application shell — deep violet-black surfaces, one magenta→violet
 * accent, mono type for data. Plain CSS keyed to the classes [[AppShell]] emits and the stable `ir-*` / `kb-*` ids the
 * views carry, so any host (Electron renderer, browser page) can adopt it by injecting one stylesheet.
 */
object Theme:

  val css: String =
    """
      |:root {
      |  --bg: #0f0d14;
      |  --surface: #16131d;
      |  --panel: #1a1622;
      |  --panel-edge: #2a2438;
      |  --text: #e8e4f1;
      |  --muted: #8d849e;
      |  --muted2: #6f6785;
      |  --accent: #d6409f;
      |  --accent2: #8b5cf6;
      |  --mono: ui-monospace, "SF Mono", Menlo, monospace;
      |}
      |* { box-sizing: border-box; margin: 0; padding: 0; }
      |html, body { height: 100%; }
      |body {
      |  background: var(--bg);
      |  color: var(--text);
      |  font: 14px/1.55 -apple-system, "Segoe UI", system-ui, sans-serif;
      |  -webkit-font-smoothing: antialiased;
      |}
      |::selection { background: rgba(214, 64, 159, 0.35); }
      |::-webkit-scrollbar { width: 10px; }
      |::-webkit-scrollbar-thumb { background: #2a2438; border-radius: 5px; }
      |
      |.app { display: flex; flex-direction: row; height: 100vh; }
      |
      |.sidebar {
      |  width: 224px; flex: none; display: flex; flex-direction: column;
      |  background: #121017; border-right: 1px solid #241f30; padding: 18px 12px;
      |}
      |.brand {
      |  display: flex; flex-direction: row; align-items: baseline; gap: 8px; padding: 2px 10px 18px;
      |  font-weight: 700; font-size: 17px; letter-spacing: -0.01em;
      |}
      |.brand-mark {
      |  background: linear-gradient(120deg, var(--accent), var(--accent2));
      |  -webkit-background-clip: text; background-clip: text; color: transparent;
      |}
      |.brand-sub { font: 600 9px var(--mono); letter-spacing: 0.22em; color: var(--muted2); }
      |.nav-section { font: 600 10px var(--mono); letter-spacing: 0.16em; color: var(--muted2);
      |  text-transform: uppercase; padding: 14px 10px 6px; }
      |.nav-item {
      |  display: flex; flex-direction: row; align-items: center; gap: 10px; padding: 8px 10px; margin: 1px 0;
      |  border-radius: 8px; color: #a89fbe; font-weight: 500; cursor: default;
      |}
      |.nav-item:hover { background: #1a1622; color: var(--text); }
      |.nav-item.active {
      |  background: linear-gradient(90deg, rgba(214, 64, 159, 0.16), rgba(139, 92, 246, 0.10));
      |  color: #fff; box-shadow: inset 2px 0 0 var(--accent);
      |}
      |.nav-dot { width: 6px; height: 6px; border-radius: 50%; background: #3d3550; flex: none; }
      |.nav-item.active .nav-dot { background: var(--accent); }
      |.sidebar-foot {
      |  margin-top: auto; padding: 6px 4px 0; display: flex; flex-direction: row;
      |  align-items: center; justify-content: space-between;
      |}
      |.icon-btn {
      |  width: 30px; height: 30px; border-radius: 8px; display: flex; flex-direction: row;
      |  align-items: center; justify-content: center; color: var(--muted); cursor: pointer;
      |}
      |.icon-btn:hover { background: #1f1a29; color: var(--text); }
      |.icon-btn svg { display: block; }
      |.foot-meta { font: 11px var(--mono); color: var(--muted2); }
      |
      |.main { flex: 1; display: flex; flex-direction: column; min-width: 0; }
      |.topbar {
      |  height: 52px; flex: none; display: flex; flex-direction: row; align-items: center; justify-content: space-between;
      |  padding: 0 22px; border-bottom: 1px solid #241f30; background: var(--surface);
      |}
      |.topbar-title { display: flex; flex-direction: row; align-items: baseline; gap: 4px; font-weight: 600; font-size: 14px; }
      |.topbar-title .crumb { color: var(--muted2); font-weight: 400; }
      |.chip {
      |  font: 600 11px var(--mono); padding: 3px 10px; border-radius: 999px;
      |  color: #f2b7dd; background: rgba(214, 64, 159, 0.14); border: 1px solid rgba(214, 64, 159, 0.35);
      |}
      |
      |.content {
      |  flex: 1; overflow: auto; padding: 22px;
      |  display: grid; grid-template-columns: repeat(auto-fit, minmax(380px, 1fr)); gap: 16px;
      |  align-content: start;
      |}
      |.panel {
      |  background: var(--panel); border: 1px solid var(--panel-edge); border-radius: 12px;
      |  padding: 16px 18px 8px; min-width: 0;
      |}
      |.panel > h2 {
      |  font: 600 10px var(--mono); letter-spacing: 0.18em; text-transform: uppercase;
      |  color: var(--muted2); padding-bottom: 12px;
      |}
      |
      |#ir-packages, #ir-modules, #kb-bundles { list-style: none; }
      |#ir-packages li, #ir-modules li, #kb-bundles li {
      |  padding: 9px 4px; border-bottom: 1px solid #221d2e; font: 13px var(--mono);
      |}
      |#ir-packages li:last-child, #ir-modules li:last-child, #kb-bundles li:last-child { border-bottom: 0; }
      |#ir-packages li:hover, #ir-modules li:hover { background: #1f1a29; }
      |
      |#kb-intents { width: 100%; border-collapse: collapse; font-size: 13px; }
      |#kb-intents th {
      |  text-align: left; font: 600 10px var(--mono); letter-spacing: 0.14em; text-transform: uppercase;
      |  color: var(--muted2); padding: 4px 10px 10px 4px; border-bottom: 1px solid var(--panel-edge);
      |}
      |#kb-intents td { padding: 9px 10px 9px 4px; border-bottom: 1px solid #221d2e; }
      |#kb-intents tr:last-child td { border-bottom: 0; }
      |#kb-intents td:first-child { font: 600 12px var(--mono); color: var(--accent2); }
      |#kb-intents td:nth-child(3) { font: 600 11px var(--mono); color: #f2b7dd; }
      |#kb-intents td:nth-child(4) { font: 12px var(--mono); color: var(--muted2); }
      |
      |#ir-definition pre {
      |  background: #131019; border: 1px solid #241f30; border-radius: 8px;
      |  padding: 12px 14px; font: 12.5px var(--mono); overflow-x: auto;
      |}
      |""".stripMargin
