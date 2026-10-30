---
type: Intent
title: Live GitHub HTTP on Scala Native
description: "Make morphir-connector-github's live client post to GitHub from Scala Native, which today fails every call with a Transport error."
state: Backlog
kind: feature
breaking: false
created: 2026-10-30
state_since: 2026-10-30
tags: [connector, github, native, kyo]
---

# 0041 — Live GitHub HTTP on Scala Native

Make morphir-connector-github's live client post to GitHub from Scala Native, which today fails every call with a Transport error.

## Problem

When a Scala Native program lists issues, pull requests or discussions through `morphir-connector-github`, it
wants the same live answers the JVM and Node.js get, so it can ship as one native binary with no JVM or Node runtime.

Today it cannot. On Scala Native, `GithubClient.live` exists so the public API is the same on every platform, but
every call fails with `GitHubException.Transport`. The module does not take `kyo-http` on Native. The cause is
upstream: the published `kyo-net` 1.0.0-RC7 Native artifact was generated on a Linux host, so its kqueue binding is
a stub that throws, and a macOS binary would have no I/O backend. Kyo 1.0.0-RC7 fixed the separate macOS link
failure ([getkyo/kyo#1896](https://github.com/getkyo/kyo/pull/1896)). Linux Native, where the epoll backend is
real, has never been tried.

This is the part of [0020](/0020-github-graphql-connector.md) that did not ship. The connector as it stands is
described by the [GitHub GraphQL connector Capability](../morphir/morphir-scala/github-graphql-connector.md), and
the constraint is recorded in the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).

## Approach

To be settled in Refinement. Two facts shape it:

- A published Kyo release whose Native artifact carries working kqueue bindings removes the macOS blocker. Until
  then, macOS Native cannot do live HTTP through `kyo-http`.
- Linux Native may already work, because its backend is not a stub. Trying it is the cheapest first step: add
  `kyo-http` to the Native module, link OpenSSL through `MorphirNativeOpenSsl`, and run the recorded-fixture tests
  plus one live call on Linux.

Whether to ship Linux first, contribute kqueue codegen to Kyo, or wait for a release is the decision Refinement
owes.
