---
type: Intent
title: Project-owned kb templates and conformance scoring
description: Let each repository carry its own document templates and its own bar for a good document, instead of hardcoding Morphir's in the kb skill.
state: Backlog
kind: docs
breaking: false
created: 2026-08-19
state_since: 2026-08-19
tags: [kb, tooling, templates, conformance]
---

# 0034 — Project-owned kb templates and conformance scoring

Each repository should carry its own document templates and its own bar for what a good document is. The kb
skill should supply the machinery and read that configuration, rather than hardcoding one project's answer.

## Problem

The kb skill is meant to be universal. Morphir is its first user, and other repositories are expected to build
their own knowledge bases with it. Two things block that today.

The first is naming, already recorded as a task: Morphir appears across the skill prose, its reference pages and
its Scala sources.

The second is that the skill hardcodes what a good document looks like, and it does so inconsistently. The
white-paper register requires five parts: conclusion first, context, argument, rejected alternatives, and an
unresolved section that it calls mandatory. `type: Intent` routes to that register. Yet `kb intent new`
scaffolds only Problem and Approach, and tells the author the Approach may be deleted if it stays trivial.

The result is measurable. Of the 33 intent records in this bundle, all 33 have an Approach, none has a rejected
alternatives section, and one has an unresolved section. Two records use rejected-alternative phrasing anywhere
in their prose. The register is not being met because nothing scaffolds it and nothing checks it.

A second repository adopting the skill would inherit that gap, and would also inherit Morphir's answer to a
question it should answer for itself. A library project, a service and a specification repository do not owe the
same sections.

## Approach

The repository owns the templates and the bar. The skill owns the machinery that reads them, scaffolds from
them, and reports against them.

Three pieces, in the order they unblock each other. A template set the repository provides, keyed by document
`type`, that `kb intent new` and any future `kb new` scaffold from. A conformance description saying which
sections each `type` owes and whether a missing one warns or errors. A report that scores documents against
that description, so a gap is visible without reading every file.

The kb skill ships a default set matching the current style cards, so a repository that configures nothing
behaves as this one does today.

Sequencing matters. Conformance scoring against templates nobody has written scores nothing, so templates come
first. This intent covers the framework. Morphir's own templates and bar are configuration authored under it,
not part of the machinery.

## Alternatives

**Keep the rules hardcoded and fix only the naming.** Considered and rejected. It closes the task already
recorded for naming and leaves the larger problem, which is that the skill asserts one project's editorial
standard. A second adopter would have to fork the skill to change a section list.

**Validate against a schema per `type`, with no templates.** Considered and rejected as insufficient on its own.
A schema catches a missing section after the fact. The measured evidence here is that authors write what the
scaffold gives them, so a check without a matching template reports the same gap 33 times instead of preventing
it.

**Score documents without configuration, using the style cards directly.** Considered and rejected. The style
cards are prose written for a reader, not a machine-readable description, and deriving a checker from them would
couple the skill to their wording. It would also keep the bar global when the requirement is that a project sets
its own.

## Unresolved

**What form the configuration takes.** A file per template against a single description, and where either sits
relative to the bundle root, is undecided. The choice interacts with how `kb refresh` discovers content.

**Whether conformance blocks.** A missing mandatory section could warn or fail `kb check`. Warning risks
repeating the current outcome, where guidance without enforcement produced no compliance. Failing risks blocking
work on records written before the rule existed. A per-section severity in the configuration would settle it,
at the cost of another thing to configure.

**How scoring reports.** Whether a score is a number, a checklist per document, or only a list of gaps is open.
A number invites gaming and hides which section is missing.

**Whether existing records are backfilled.** 31 intent records lack the sections the register requires. Nobody
now holds the knowledge of what was considered and rejected for most of them, and inventing it would be worse
than leaving it absent, because a fabricated rejected alternative is the sentence a reader trusts most. Whether
those records are backfilled when next touched, marked as predating the rule, or left alone is undecided.

## Relationships

Depends on the task to remove Morphir-specific naming from the kb skill, which shares this intent's goal of
making the skill adoptable elsewhere.
The registers and the bar this intent makes configurable are described in
[kb/AGENTS.md](../../AGENTS.md) and the style cards it names.
The vocabulary is in [kb/CONTEXT.md](../../CONTEXT.md).
