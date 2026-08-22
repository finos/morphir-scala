---
type: Intent
title: Local web host and GitHub connection settings
description: "Let browser and Electron users submit a GitHub.com token to the host for validation and optional operating-system persistence without returning it to the client."
state: InProgress
kind: feature
breaking: false
created: 2026-08-21
state_since: 2026-08-22
tags: [github, ui, web, electron, appkit]
---

# 0036: Local web host and GitHub connection settings

Let browser and Electron users submit, validate, remember, and remove a GitHub.com token without exposing it
outside the host process.

## Problem

The GitHub connector can read tokens from flags, GitHub Actions, `gh`, and an existing `SecretStore` entry. The UI
has no way to establish that entry or a session-only connection. A browser user would otherwise have to put a token
in browser storage or configure the CLI outside the UI. An Electron user would have to configure a separate token
source even though the desktop app already has an encrypted secret-store reader.

The browser and Electron renderer are untrusted relative to their host process. They must submit a token once, but
they must not retain it, use it for GitHub calls, or receive it again. The host must validate the token before it
replaces a working connection. Remembering a token must use an operating-system credential facility and must remain
an explicit choice.

## Approach

Add one transport-blind `GitHubConnectionService` to `morphir-ui`. It accepts a redacted `TokenSubmission` and
returns only connection status. A connection reports the GitHub login and whether it lasts for the session or is
remembered on the device. The shared settings view defaults to session-only use.

Both hosts implement the same service. Electron handles it in the main process over the existing JSON-RPC IPC
transport. A new Kyo JVM web host serves the browser UI and JSON-RPC from one loopback origin. The existing Kyo
CaseApp CLI adds `morphir serve` and runs this host directly. It does not route through the legacy ZIO dispatcher or
add a ZIO-to-Kyo bridge.

Extend appkit with a writable `SecretVault`. The JVM host uses the operating-system credential store. Electron uses
the asynchronous `safeStorage` API and persists only ciphertext. Electron refuses remembered storage when Linux
selects its `basic_text` backend. Both hosts keep session tokens only in process memory.

The host parses the submission, asks the GitHub connector to validate it against GitHub.com, persists it when
requested, then changes the active provider. A failure leaves the previous connection intact. Disconnect removes
the active token and its remembered copy.

The full design is [GitHub connection settings and local web host](../morphir/morphir-scala/design/github-connection-settings-and-local-web-host.md).

## Scope

This intent includes GitHub.com, pasted personal access tokens, the shared settings UI, secure optional persistence,
Electron hosting, the loopback web host, and the `morphir serve` command.

GitHub Enterprise Server is follow-up work because the current connector fixes its live endpoint to
`https://api.github.com/graphql`. OAuth, device flow, remote binding, and multi-user deployment are also outside this
intent.

## Alternatives

**Store the token in the renderer.** Considered and rejected. A browser would need to retain the token in memory or
web storage and make GitHub calls itself. Electron would have to expose encryption or filesystem operations to its
renderer.

**Give each host its own credential protocol.** Considered and rejected. A dedicated web endpoint plus a separate
Electron preload API duplicates behavior and makes the shared UI depend on host adapters. The existing JSON-RPC
contract already carries host operations without exposing Electron APIs.

**Run the web host through the legacy ZIO command dispatcher.** Considered and rejected. New code uses Kyo under
[Decision Record 0005](../morphir/morphir-scala/decisions/0005-bridge-nothing-between-zio-and-kyo.md). The current CLI
entry point can run a new Kyo command without entering the legacy dispatcher.

## Unresolved

The implementation must pass a focused security audit before the capability is considered release-ready. That audit
must challenge the loopback request controls, token serialization, Electron IPC, operating-system persistence,
logging, and process-memory lifetime.

A later design must choose between OAuth authorization-code flow with PKCE and device flow. GitHub currently requires
an OAuth app client secret for authorization-code exchange, while device flow has different phishing risks. Neither
choice belongs in the pasted-token release.
