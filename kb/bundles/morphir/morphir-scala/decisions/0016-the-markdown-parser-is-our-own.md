---
type: Decision Record
title: "The Markdown parser is our own, permanently"
description: "morphir-langkit-markdown implements and keeps its own CommonMark and GFM parser; replacing it with a third-party engine is no longer under consideration."
state: Accepted
decided: 2026-08-22
tags: [langkit, markdown, parser, commonmark, gfm, dependencies]
status: stable
---

# 0016 — The Markdown parser is our own, permanently

`morphir-langkit-markdown` implements its own Markdown parser and keeps it. Replacing it with a third-party
engine is no longer under consideration, and the standing allowance that one "remains allowed later if it
compiles on all three platforms" is withdrawn. `commonmark-java` must not enter the module, and no other engine
takes its place.

## Summary

The parser now measures its own conformance and carries tree machinery no third-party engine supplies, so
replacing it would give up properties the rest of the module depends on. This decision keeps the parser in
place permanently and withdraws the earlier allowance to swap it out later.

| Option | Outcome | Why |
| --- | --- | --- |
| Keep the parser we wrote, permanently | Chosen | It measures full conformance against CommonMark 0.31.2 and 662 of 663 GitHub Flavored Markdown examples, and its tree machinery is what consumers depend on. |
| Keep the standing allowance to replace it later | Rejected | It framed parser work as a stopgap and misled reviews about its value. |
| Wrap a different engine per platform | Rejected | Three engines under one facade cost more than one parser and still could not produce the concrete syntax tree. |

## Why

The allowance made sense when the parser was a subset and the module's value was uncertain. Neither is true any
more, and three facts settle it.

**Conformance is measured, not hoped for.** The parser scores 652 of 652 against CommonMark 0.31.2 and 662 of
663 against the GitHub Flavored Markdown suite, measured on every build, and a regression fails it, with the
one open example tracked as work and nine divergences recorded with reasons. The original motive for allowing a
replacement was doubt that an in-house parser could be trusted; the suite now answers that question every
build, byte for byte.

**The module's value is no longer the parse alone.** What consumers depend on is the machinery around it: a
concrete syntax tree whose leaves tile the source exactly and print back byte for byte, an abstract syntax tree
lowered from it, source spans on every inline node, an authoring DSL, a Markdown writer whose output re-reads
as the same tree, and profile-gated dialect extensions that branch at the earliest stage whose output tree can
record them ([decision 0015](/decisions/0015-profile-branches-at-the-earliest-capable-stage.md)). A third-party
engine supplies none of that. It hands over its own abstract tree — no byte-exact concrete tree, no tiling
invariant, no spans in our vocabulary, no seat for the placement rule — so an engine swap would not replace the
parser; it would forfeit the properties the rest of the module is built on.

**The portability constraint never went away.** The parser exists because `commonmark-java` is JVM-only and the
langkit targets the JVM, Scala.js and Scala Native. That constraint eliminated every mainstream engine in 2026
and still does; an engine that appeared tomorrow on all three platforms would still fail the second point.

## Alternatives rejected

### Keeping the allowance

Costless-looking, but it was already misleading readers: it framed the parser as a stopgap, inviting effort
estimates and reviews to treat parser work as throwaway. A module whose CST invariants consumers depend on
cannot honestly advertise that its parse might be swapped out.

### Wrapping an engine per platform

Three engines with three behaviours under one facade, reconciled against a byte-exact conformance suite, is
strictly more work than one parser — and the facade still could not produce the concrete syntax tree.

## Consequences

Parser work is maintenance of a permanent asset, not investment in a placeholder. Conformance regressions are
build failures. The module's `README.md` states the settlement, and intent 0021's allowance is amended to match.
Dependency review keeps `commonmark-java` and equivalents out of `morphir-langkit-markdown`; the kb skill's own
JVM-side use of `commonmark-java` elsewhere in the repository is unaffected and out of scope.

## Revisit when

The langkit's platform set changes so that maintaining the parser becomes infeasible, or the CommonMark
specification changes so substantially that re-conforming costs more than the module's tree machinery is worth.
Nothing short of those reopens this.
