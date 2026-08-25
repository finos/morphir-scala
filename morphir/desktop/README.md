# morphir-desktop

Unpublished Electron desktop app for Morphir: `morphir-ui` mounted in a sandboxed renderer, talking
to Scala.js main-process services over the `morphir-appkit-electron` JSON-RPC seam. Proof-of-stack
first release: IR explorer and knowledge/intent browser over fixed demo data.

- `main/` — Electron main process (Scala.js, CommonJS): bootstrap, demo services, RPC handler.
- `renderer/` — browser bundle (Scala.js script): bridge port, RPC client, kyo-ui mount.
- `app/` — static Electron app dir: `package.json`, `preload.cjs`, `index.html`; linked bundles land
  in `app/dist/` (gitignored).
- `scripts/run.sh` — link, assemble, launch. `scripts/smoke.sh` — run `morphir.desktop.smokeRun`, which validates 18
  assertions across multiple GitHub connection-state transitions.
- `scripts/package.sh <platform-token> <version>` — release packaging: links with `fullLinkJS` and runs
  electron-builder for one platform, leaving raw output in `app/release/`. CI calls this per runner; the
  canonical naming and checksums are applied later by `ci.desktop.canonicalize`.

The renderer never touches Electron APIs: `contextIsolation` stays on, and the only bridge is the
`morphirIpc` postMessage/onMessage pair exposed by the hand-written preload. See kb intent 0030.

## GitHub connection settings

The desktop Connections panel accepts GitHub.com personal access tokens through the existing JSON-RPC bridge. The
token stays in the main process after submission. Remember this device is unchecked by default, so ordinary
connections live only for the running desktop session. Disconnect drops that session token and removes any
remembered credential.

Electron remembers a token only when `safeStorage` reports encryption is available. On Linux, Morphir refuses
remembered storage when Electron selects `basic_text` or cannot identify a secure backend. It then continues to
offer session-only connections. A remembered connection that cannot be written leaves the previous connection in
place and reports the failure; it does not become a session connection without the user's choice.

## Downloading a release

Every release asset is published twice, under the same filename:

- GitHub Releases — archives, installers, a `.sha256` per asset, and a GPG-signed `checksums.txt`.
- Maven Central — the portable archives only, one artifactId per platform:
  `org.finos.morphir:morphir-desktop-<os>-<arch>:<version>`.

The platform tokens are `mac-aarch64`, `mac-amd64`, `linux-amd64`, `linux-aarch64` and `win-amd64`,
matching Mill's native launcher naming.

Verify what you download. A per-asset `.sha256` sidecar only proves the file was not corrupted in
transit — an attacker able to replace an asset can replace its sidecar too — so treat this as a first
check, not the whole story:

```bash
sha256sum -c --ignore-missing checksums.txt
```

`--ignore-missing` skips entries for assets you did not download, so checking one file does not fail
on the other eleven listed in `checksums.txt`.

The stronger check is the detached GPG signature over `checksums.txt`, made with the key that signs
Morphir's Maven Central artifacts (fingerprint `2EEC FCE1 591B 5738 B39C 6F8D 6EE7 E9F9 A7EC 903E`,
verified against published `.jar.asc` signatures under `org.finos.morphir`). **That key is not yet
published to a public keyserver** — `keys.openpgp.org` holds it without a verified user ID, and
`keyserver.ubuntu.com` has no record of it at all — so `gpg --recv-keys` cannot fetch a usable copy
today. Once it is published, importing and verifying looks like this:

```bash
gpg --keyserver keys.openpgp.org --recv-keys 2EECFCE1591B5738B39C6F8D6EE7E9F9A7EC903E
gpg --verify checksums.txt.asc checksums.txt
```

Until then, you can still run `gpg --verify checksums.txt.asc checksums.txt` without importing
anything: gpg reports the signing key id even when it cannot complete verification. Compare that id
against the last 16 hex digits of the fingerprint above (`6EE7E9F9A7EC903E`). This is weaker than a
full verification — it confirms which key signed, not that the key belongs to this project — but it
is the only signature-based check available until the key is on a keyserver.

Builds are unsigned until code-signing certificates are in place. On macOS, clear the quarantine
attribute after verifying the checksum:

```bash
xattr -dr com.apple.quarantine /Applications/Morphir\ Desktop.app
```
