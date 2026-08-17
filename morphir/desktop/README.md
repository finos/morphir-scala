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

## Downloading a release

Every release asset is published twice, under the same filename:

- GitHub Releases — archives, installers, a `.sha256` per asset, and a GPG-signed `checksums.txt`.
- Maven Central — the portable archives only, one artifactId per platform:
  `org.finos.morphir:morphir-desktop-<os>-<arch>:<version>`.

The platform tokens are `mac-aarch64`, `mac-amd64`, `linux-amd64`, `linux-aarch64` and `win-amd64`,
matching Mill's native launcher naming. Verify a download before running it:

```bash
sha256sum -c morphir-desktop-mac-aarch64-<version>.zip.sha256
gpg --verify checksums.txt.asc checksums.txt
```

Builds are unsigned until code-signing certificates are in place. On macOS, clear the quarantine
attribute after verifying the checksum:

```bash
xattr -dr com.apple.quarantine /Applications/Morphir\ Desktop.app
```
