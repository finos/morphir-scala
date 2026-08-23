---
type: Intent
title: Morphir IR and Morphir test tooling as a conformance spec source
description: "Express the Markdown conformance fixtures as Morphir values executed by Morphir test tooling, so the langkit is exercised by the system it belongs to."
state: Backlog
kind: test
breaking: false
created: 2026-08-23
state_since: 2026-08-23
tags: [langkit, markdown, conformance, ir, testing]
---

# 0036 — Morphir IR and Morphir test tooling as a conformance spec source

Express the Markdown conformance fixtures as Morphir values executed by Morphir test tooling, so the langkit is
exercised by the system it belongs to.

This records a direction, not scheduled work. It sits in Backlog until its prerequisites are met, and Backlog is
where it belongs: nothing about the conformance suite is wrong today, and nothing here changes what the suite
measures.

## Problem

The Markdown conformance suite ([0021](/0021-markdown-langkit.md), [0035](/0035-github-flavored-markdown-profile.md))
reads its fixtures from the CommonMark project's published `spec.json` and the GFM specification text. That is the
right source today: the fixtures are upstream's own examples, vendored exactly, and the scores they produce are
comparable to every other implementation that measures against the same files.

What the suite does not do is exercise Morphir itself. Morphir's reason to exist is capturing definitions as data —
and a conformance fixture is exactly that: an input, an expected output, and a name. A suite whose fixtures are
Morphir values, executed by Morphir test tooling, would make the langkit a consumer of the system it belongs to,
and the conformance run a standing proof that the IR and the test tooling can carry a real workload end to end.

## Approach

Four things must fall into place first; none is scheduled by this record:

1. Morphir IR v4 stable enough to encode fixtures against — the open v4 issues bear on this.
2. A way to express a Markdown fixture and its expected output as Morphir values.
3. Cross-platform IR loading: the suite runs on JVM, JS and Native, and today there is no portable file read.
4. Morphir test tooling able to drive a suite of that shape.

One design consequence applies now rather than later: when the conformance harness is next reworked, fixture
ingestion goes behind a `SpecSource` seam, so an IR-backed source arrives as an added implementation rather than a
rewrite of the harness. No such seam exists yet; this is the constraint on whoever touches ingestion first.

This intent leaves Backlog when the prerequisites are met and a first slice is written against them, or it is
cancelled with a reason if the target is deliberately abandoned.
