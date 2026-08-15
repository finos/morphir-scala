---
type: Intent
title: GitHub CLI connector
description: "Publish a gh-process wrapper as morphir-connector-github-cli, which may be JVM-only."
state: Backlog
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [connector, github, cli]
---

# 0024 — GitHub CLI connector

Publish a gh-process wrapper as morphir-connector-github-cli, which may be JVM-only.

## Problem

Some Morphir tools already run in an environment that has the `gh` binary and a logged-in user. Calling GraphQL
directly then duplicates auth and pagination that `gh` already handles. A published wrapper would let those tools
share one process integration without depending on the GraphQL module's HTTP stack.

`gh` is a process, not a library, so this is a connector. It is not a package inside the GraphQL module, because
that module must not grow a process dependency.

## Approach

Publish `morphir/connector/github-cli` as a sibling of `morphir/connector/github`. The module may be JVM-only; that
exception is named here and does not change the GraphQL connector's JVM plus JS plus Native rule.
See [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).

Shared GitHub-shaped types, if any, come from the GraphQL connector or from a later `github-core` split. This intent
does not require that split.

Out of scope until [0020](/0020-github-graphql-connector.md) has a working client.

The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
