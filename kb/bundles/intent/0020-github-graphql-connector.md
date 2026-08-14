---
type: Intent
title: GitHub GraphQL connector
description: "Publish a Kyo GitHub GraphQL client as morphir-connector-github, with no Morphir types, on JVM, JS, and Native."
state: Refinement
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
The public surface is a token, a typed error ADT, GitHub-shaped issue, pull request, and discussion types, and a
client that lists those objects for a repository. No OKF type and no Morphir IR type appears in the module.

The client consumes GitHub's GraphQL API. `kyo-caliban` is a GraphQL server and is out of scope. Generated Scala
comes from `caliban-client` against a vendored **subset** of GitHub's schema, checked in and produced by a documented
command. REST is used only for endpoints GraphQL lacks.

The HTTP stack (`kyo-http` or sttp wrapped in Kyo) must run on JS and Native, not merely compile. Until that check
is recorded in the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md), the module
uses a fixture-backed client and takes neither dependency. Tests replay recorded GraphQL fixtures and do not call
`api.github.com`.

The `gh` CLI is [0024](/0024-github-cli-connector.md), a sibling module, not a package here.

Depends on [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md). The narrative home
is the [published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
