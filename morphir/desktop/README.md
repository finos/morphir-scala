# morphir-desktop

Unpublished Electron desktop app for Morphir: `morphir-ui` mounted in a sandboxed renderer, talking
to Scala.js main-process services over the `morphir-appkit-electron` JSON-RPC seam. Proof-of-stack
first release: IR explorer and knowledge/intent browser over fixed demo data.

- `main/` — Electron main process (Scala.js, CommonJS): bootstrap, demo services, RPC handler.
- `renderer/` — browser bundle (Scala.js script): bridge port, RPC client, kyo-ui mount.
- `app/` — static Electron app dir: `package.json`, `preload.cjs`, `index.html`; linked bundles land
  in `app/dist/` (gitignored).
- `scripts/run.sh` — link, assemble, launch. `scripts/smoke.sh` — headless boot + one RPC round-trip.
- `scripts/package.sh <platform-token> <version>` — release packaging: links with `fullLinkJS` and runs
  electron-builder for one platform, leaving raw output in `app/release/`. CI calls this per runner; the
  canonical naming and checksums are applied later by `ci.desktop.canonicalize`.

The renderer never touches Electron APIs: `contextIsolation` stays on, and the only bridge is the
`morphirIpc` postMessage/onMessage pair exposed by the hand-written preload. See kb intent 0030.
