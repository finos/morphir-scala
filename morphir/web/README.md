# Morphir local web host

`morphir.web.server` binds the browser settings application to `127.0.0.1`. Port `0` asks the operating system for
an available port. The host does not support remote interfaces, alternate host names, or GitHub Enterprise Server.

At startup the host creates one launch credential. The default launcher receives it in a URL fragment, exchanges it
once for an HttpOnly same-site cookie, and removes the fragment before any other request. The host never prints the
credential. With browser opening disabled, the first valid request to the printed loopback origin receives the same
fragment as a one-time relative redirect.

The renderer serves only the tracked index and stylesheet plus the Scala.js file linked by Mill. The generated
JavaScript remains a build output under `out/` and is not committed; the production link disables source-map output.

## Running the local host

Run the browser application with:

```text
morphir serve
```

It binds `127.0.0.1` only and asks the operating system to choose a port by default. It then opens the browser.
Use `morphir serve --no-open` when the browser should not be opened automatically, or `morphir serve --port <port>`
to request a specific loopback port. The command prints the loopback origin but never the one-use launch credential.

The Connections settings panel accepts GitHub.com personal access tokens. Create a token in
[GitHub's personal access token settings](https://github.com/settings/tokens) and grant only the access needed for
the repositories or organizations you use. The Remember this device control starts unchecked. A regular connection
therefore lasts only while `morphir serve` runs. If a remembered write fails, the host retains the previous
connection and reports the failure. It does not silently switch to session-only use. Disconnect removes the active
token and the remembered operating-system credential, if one exists.

GitHub Enterprise Server is not supported by this host; follow-up `morphir-sx3` owns hostname selection and
Enterprise credential isolation. Server shutdown finalization has a separate upstream Kyo follow-up,
`morphir-3h7`; it does not change connection behavior, but it remains relevant to operators supervising a stopped
`morphir serve` process.
