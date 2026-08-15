---
type: Intent
title: GitHub GraphQL connector
description: "Publish a Kyo GitHub GraphQL client as morphir-connector-github, with no Morphir types, on JVM, JS, and Native."
state: InProgress
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [connector, github, kyo]
---

# 0020 — GitHub GraphQL connector

Publish a Kyo GitHub GraphQL client as morphir-connector-github, with no Morphir types, on JVM, JS, and Native.

## Problem

When tooling or a library user needs issues, pull requests, or discussions from GitHub, there is no published Morphir
client to call. Each caller would otherwise write its own HTTP and GraphQL layer, and those copies would not share
auth, error types, or a schema subset. GitHub is an external system, not a Scala library Morphir builds on, so the
client does not belong in `kit`.

## Approach

Publish `morphir/connector/github` as `org.finos.morphir::morphir-connector-github`, compiling for JVM, JS, and Native.
The public surface is a redacted token class, a typed error ADT, GitHub-shaped issue, pull request, and discussion types, and a
client that lists those objects for a repository. Live calls take `Env[TokenProvider]`. The host installs one named provider
(const, kyo `StaticFlag` named `token`, `GITHUB_TOKEN` via `gitHubActions`, `gh auth token` with optional user and hostname, or an appkit `SecretStore` adapter) as a Kyo `Layer`. See
[GitHub token providers and appkit secrets](../morphir/morphir-scala/design/github-token-providers-and-appkit-secrets.md).
No OKF type and no Morphir IR type appears in the module.
Listing includes author, UTC timestamps as `Maybe[java.time.Instant]`, labels, and comments.
Discussions also include upvoteCount, an accepted answer, and nested comment replies.
`listDiscussions` takes a `ReplyDepth` (default one level). Listing methods return `ConnectionPage` and take `after`
and `first` so a caller can page. `listDiscussionReplies` pages further replies by comment id.
`getIssue`, `getPullRequest`, and `getDiscussion` look up one object by repository number and return `Maybe`.
Issue and pull request comments have no upvoteCount.

The client consumes GitHub's GraphQL API. `kyo-caliban` is a GraphQL server and is out of scope. Generated Scala
comes from `caliban-client` against a vendored **subset** of GitHub's schema, checked in and produced by the Mill
script `morphir/connector/github/schema/gen-client.scala`. REST is used only for endpoints GraphQL lacks.

`kyo-http` 1.0.0-RC6 is the HTTP stack. Live POST runs on the JVM and on Node.js. The github JS module sets
`ModuleKind.CommonJSModule` so Scala.js can import Node builtins (`node:fs`, `node:net`, `node:tls`). kyo-http on JS
is Node-only. A browser `fetch` backend is out of scope: kyo-http has no fetch floor, and GitHub GraphQL from a page
origin is a CORS and token problem. Electron uses the Node backend. Scala Native stays stubbed: the published kyo-net
1.0.0-RC6 Native artifact was generated on Linux, so kqueue is a throwing stub and epoll/io_uring do not link on
macOS. Tests replay recorded GraphQL fixtures and do not call `api.github.com`. The check lives in the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).

The `gh` CLI is [0024](/0024-github-cli-connector.md), a sibling module, not a package here.

Depends on [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md). The narrative home
is the [published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
