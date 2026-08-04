---
type: Decision Record
title: Decision Records are a third register in the knowledge base
description: "Architectural decisions are recorded as `type: Decision Record` concepts that are superseded rather than edited, alongside Intent and Capability."
state: Accepted
decided: 2026-07-30
tags: [kb, decisions, intent, capability, process]
status: stable
---

# 0004 — Decision Records are a third register in the knowledge base

Architectural decisions live in the knowledge base as concepts with `type: Decision Record`, numbered `NNNN-slug.md`,
in a `decisions/` directory inside the bundle they concern. They carry `state` (Proposed, Accepted, Superseded,
Withdrawn), a `decided` date, and `supersedes`/`superseded_by` links. A decision is never rewritten once accepted; it
is superseded by a later record.

They are not stored in `docs/adr/`, which is where the first three lived before this decision moved them.

## Why

The knowledge base already had two registers and neither fits.

An **Intent** is future-tense and has a lifecycle; it answers *should we do this*. A **Capability** is present-tense
and has no lifecycle; it answers *what does the system do*. Neither answers *why is it shaped this way* — and that
question outlives both. The intent that produced a module is closed and the capability describing it says nothing
about the three alternatives that were rejected, so a reader who wants to know whether they may change something has
nowhere to look. The kb glossary's definition of Intent ("a recorded decision about work the project means to do")
already claims the word *decision*, which is precisely why the third register needs a different name rather than an
extension of that one.

The closer call was **Design Note**, which already exists — eleven of them describe the draft IR v4 format, and
`intent check` already treats a Design Note as the release target for a spike. The distinction that decided it is
mutability. A Design Note is updated as understanding improves; that is what makes it useful. A Decision Record must
*not* be, because its value is the reasoning available at the time, which is exactly what an edit destroys. Once the
conclusion changes you want both documents, not one document that has quietly changed its mind. That is a different
enough contract to justify a separate type, and it is why `supersedes`/`superseded_by` are modelled and checked for
mutual consistency rather than left to prose.

Keeping them in `docs/adr/` was the cheaper option and was rejected. Decisions are knowledge, and the knowledge base
is where this project keeps knowledge that has settled — leaving them outside it meant a reader had two places to
look, an index that knew about one of them, and no link checking, staleness or search across the other. `docs/*` is
also `linguist-vendored`, so the records were being collapsed in diffs.

## Consequences

The kb skill grew a register: `KbDecision.scala` alongside `KbIntent.scala`, nine checks wired into `kb check`, and
`kb decision list` / `kb decision show`. `Severity` and `Finding` moved from `KbCheck.scala` into `KbModel.scala` so
that a module producing findings need not depend on the check runner.

`Frontmatter.ProducerKnown` was introduced to hold the keys this tooling defines on top of OKF. It is deliberately
separate from `Frontmatter.Known` so the distinction between *what the spec says* and *what we added* stays honest.
A side effect: the 114 info-level `frontmatter-unknown-key` findings that intent's own keys had been producing went
to zero, which makes `kb check --verbose` readable again.

Numbering is per-bundle, not global. Two bundles may each start at 0001, and `kb check` enforces uniqueness only
within a bundle. This is the condition under which the scheme would need revisiting: if decisions ever need to be
cited across bundles by bare number, the ids would have to become globally unique or fully qualified.

`docs/adr/` now holds only a pointer. Old links to `docs/adr/0001` break — accepted, because there are three of them
and they are all inside this repository.
