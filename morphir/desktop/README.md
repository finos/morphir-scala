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
matching Mill's native launcher naming.

A per-asset `.sha256` sidecar only proves the file was not corrupted in transit — an attacker able to
replace an asset can replace its sidecar too. The trust anchor is the detached GPG signature over
`checksums.txt`, made with the same key that signs Morphir's Maven Central artifacts (fingerprint
`2EEC FCE1 591B 5738 B39C 6F8D 6EE7 E9F9 A7EC 903E`). Verify the signature first, then check the asset
against the signed manifest:

```bash
gpg --keyserver keys.openpgp.org --recv-keys 2EECFCE1591B5738B39C6F8D6EE7E9F9A7EC903E
gpg --verify checksums.txt.asc checksums.txt
sha256sum -c --ignore-missing checksums.txt
```

`--ignore-missing` skips entries for assets you did not download, so verifying one file does not fail
on the other eleven listed in `checksums.txt`.

Builds are unsigned until code-signing certificates are in place. On macOS, clear the quarantine
attribute after verifying the checksum:

```bash
xattr -dr com.apple.quarantine /Applications/Morphir\ Desktop.app
```
