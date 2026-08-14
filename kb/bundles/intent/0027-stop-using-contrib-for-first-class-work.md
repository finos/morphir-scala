---
type: Intent
title: Stop using contrib for first-class work
description: "Treat morphir/contrib as a parking lot: no new first-class libraries land there, and existing ones migrate or retire under later intents."
state: Backlog
kind: deprecation
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [contrib, modules]
---

# 0027 — Stop using contrib for first-class work

Treat morphir/contrib as a parking lot: no new first-class libraries land there, and existing ones migrate or retire
under later intents.

## Problem

`morphir/contrib/knowledge` is a published microkanren library sitting in a directory whose name tells contributors
that first-class work can land there. New OKF and GitHub work would have gone there by default. That hides libraries
users are meant to depend on, and it collides with OKF in the word "knowledge".

Deprecation is the announcement, not the move. The microkanren module still exists.

## Approach

Stop adding first-class modules under `morphir/contrib`. Document the directory as a parking lot in the kit and
AGENTS trees. A later intent migrates `contrib/knowledge` (microkanren) to a first-class path or retires it. This
intent does not perform that move.

OKF work goes to `morphir/knowledge/okf` under [0022](/0022-okf-knowledge-library.md).

The family rule is [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).
