---
type: Intent
title: GitHub Flavored Markdown profile
description: "Recognize the five GitHub Flavored Markdown extensions behind an opt-in profile, measured against the GFM specification's own example set."
state: InProgress
kind: feature
breaking: true
created: 2026-08-22
state_since: 2026-08-22
tags: [langkit, markdown, gfm, commonmark, conformance, profile]
sources:
  - id: gfm-spec
    resource: https://github.github.com/gfm/
    title: GitHub Flavored Markdown Spec, version 0.29-gfm
  - id: gfm-spec-text
    resource: https://raw.githubusercontent.com/github/cmark-gfm/master/test/spec.txt
    title: The GFM specification source, 672 examples, published as text only
  - id: commonmark-0-31-2
    resource: https://spec.commonmark.org/0.31.2/spec.json
    title: CommonMark 0.31.2 fixtures, 652 examples
  - id: commonmark-0-29
    resource: https://spec.commonmark.org/0.29/spec.json
    title: CommonMark 0.29 fixtures, 649 examples — the release GFM's base is drawn from
  - id: commonmark-0-28
    resource: https://spec.commonmark.org/0.28/spec.json
    title: CommonMark 0.28 fixtures, 624 examples
  - id: commonmark-0-27
    resource: https://spec.commonmark.org/0.27/spec.json
    title: CommonMark 0.27 fixtures, 622 examples
  - id: mdast
    resource: https://github.com/syntax-tree/mdast/blob/main/readme.md
    title: mdast, the Markdown abstract syntax tree vocabulary this AST follows
---

# 0035 — GitHub Flavored Markdown profile

Recognize the five GitHub Flavored Markdown extensions behind an opt-in profile, measured against the GFM
specification's own example set.

The profile is a set of features, not a dialect version. Each extension is recognized at the earliest pipeline
stage whose output tree can record the decision, so the concrete and abstract syntax trees never disagree about
what a document contains. Nine of the specification's own examples are recorded as divergences rather than
targets, because matching them would mean regressing CommonMark conformance to satisfy a behaviour no released
CommonMark ever specified and GitHub itself no longer exhibits.

## Problem

[0021](/0021-markdown-langkit.md) parses CommonMark completely and [0033](/0033-markdown-compilation.md) compiles
it: 652 of 652 examples in the CommonMark 0.31.2 suite render byte-identically through the ScalaTags oracle.

Almost no Markdown in this project's world is plain CommonMark. README files, issue bodies, pull request
descriptions and the knowledge base's own concept prose are written in GitHub Flavored Markdown, and a table or a
task list in any of them parses today as a paragraph of pipes and brackets. `morphir-knowledge-okf` reads concept
bodies through this parser; `morphir-ui` and the desktop application display them. Each of those surfaces
silently renders less than the author wrote.

The gap is measurable, because GFM publishes a conformance suite. Against its 672 examples the parser scores 641.
Twenty-two of the thirty-one failures are the five unimplemented extensions — pipe tables, extended autolinks,
task list items, strikethrough, and disallowed raw HTML. The remaining nine are a different thing, and finding
out what took measuring four CommonMark releases.

### The nine emphasis examples

All nine concern nested strong delimiters — `****foo****` and kin — where the GFM suite expects a single `strong`
element and this parser produces two nested ones. The obvious reading is version drift: GFM's base is CommonMark
0.29, this parser targets 0.31.2, and emphasis rules changed somewhere between. That reading is wrong. Every
published CommonMark release agrees with what this parser produces.

| Source | `****foo****` |
| --- | --- |
| CommonMark 0.27 | `<strong><strong>foo</strong></strong>` |
| CommonMark 0.28 | `<strong><strong>foo</strong></strong>` |
| CommonMark 0.29 | `<strong><strong>foo</strong></strong>` |
| CommonMark 0.31.2 | `<strong><strong>foo</strong></strong>` |
| GFM 0.29-gfm | `<strong>foo</strong>` |

The GFM specification text is a snapshot of cmark-gfm as it stood in April 2019, and these nine entries record an
emphasis behaviour that no CommonMark release specified. Current cmark-gfm tracks upstream cmark, so GitHub does
not render this way either. Matching them would mean regressing the CommonMark profile to satisfy a stale
document.

### Two further measurements that shaped the answer

**CommonMark releases do not disagree with each other on any shared input.** Across the 628 sources present in
both the 0.29 and 0.31.2 suites, the expected HTML is identical every time. The delta between those releases is
examples added (25) and examples removed (21), never an expectation changed. This parser scores 647 of 649
against the 0.29 suite unchanged, and both failures are the HTML-comment rule that 0.30 relaxed — a rule GFM's own
text has already adopted.

There is therefore no behavioural layering between CommonMark releases to express, and version is not an axis a
profile needs.

**The GFM suite is mostly a copy of its base.** Of its 650 non-extension entries, 636 are byte-identical to a
CommonMark 0.29 entry and 3 to a CommonMark 0.31.2 entry, GFM having already absorbed some post-0.29 fixes. The
layering the suite needs is in how a score is *reported*, not in how fixtures are *stored*.

## Approach

### A profile carries features, not a version

`MdProfile` already holds what a parse recognizes beyond CommonMark, as a set of frontmatter kinds. It gains a
second set, of `MdExtension`, and `MdProfile.gfm` is the profile enabling all five.

The enum is named for the axis rather than the dialect. GitHub's own unspecified additions — footnotes, alerts,
math — and any other dialect's extensions join the same enum rather than start a second one, so what a profile
enables stays one set whatever named the extension first. Each case carries the tag the specification's example
fences use, which is also cmark-gfm's registered extension name, so a conformance harness can report an
extension's score without knowing how the extension is implemented.

Per-extension configuration is a recorded seat and not a built one. Every value such a map would hold — the tag
filter's tag list, the autolink scheme set, the strikethrough tilde rule — is pinned by the specification, so
each key would ship with exactly one conformant setting. When a consumer needs it, it arrives as a third
defaulted field on the profile and not as state on the enum: an enum case is a singleton shared by every profile,
so a map on it would be shared too, and parameterizing the cases would cost set membership and with it the
ability to ask whether an extension is on at all. Writer-side spellings are already served by `MdStyle`.

### One placement rule

The profile reaches only the block phase today. Frontmatter works anyway, and the reason generalizes into the
rule this intent applies throughout:

> A profile decision made at an earlier stage is recorded in the tree it emits, so later stages need no profile.
> Branch at the earliest stage whose output tree can record the decision, and no later.

Lowering handles a frontmatter node with no idea whether frontmatter was enabled, because the concrete syntax
tree already answered by having the node or not. Branching later than the earliest capable stage means an earlier
tree disagrees with a later one, which is the divergence the CST round-trip tests exist to catch.

Threading the profile through inline parsing and lowering makes five locations available:

| Stage | Records a decision by | Profile |
| --- | --- | --- |
| Block parse | emitting a CST fragment kind | has it |
| Inline parse | emitting a phrasing node with a span, which the CST then tiles | added |
| CST to CST | rewriting nodes; source spans go stale | not needed |
| CST to AST | producing a different AST node for the same CST | added |
| AST to AST | rewriting the AST; the CST is left behind | not needed |

The inline stage is not the AST's alone. The CST is materialized *from* inline nodes' spans, so a node the inline
stage emits is a node both trees get. The AST-to-AST stage is where CST fidelity is deliberately given up; that
is right for what GitHub does *after* rendering — mentions, issue references, emoji, heading anchors — and wrong
for specified syntax. It is named here and not built.

Placement follows directly. Tables and task list items are block constructs and are decided at block parse.
Strikethrough is a delimiter run on the machinery emphasis already uses, and extended autolinks are a scan over
produced phrasing content; both are decided at inline parse. Disallowed raw HTML is the one with no tree-shape
change at all — the parse is byte-identical and only the meaning of the tag differs — so it is decided at
lowering, where source form becomes meaning. The CST keeps `<script>` verbatim, the AST carries the escaped text,
and all three writers stay unaware of the profile.

### Trees and algebra

The AST takes mdast's vocabulary with one deliberate deviation. mdast keeps a table's header as the first entry
of its children; GFM requires a header row, so it becomes its own field and a headerless table stops being
representable.

`Compiler[Out]` gains plain abstract methods, and all three writers are updated in the same change. That is the
module's standing rule — a node kind lands in the algebra and every writer at once — and it is what makes this
intent a compatibility break: an external implementor of the algebra will not compile until it fills them in, and
`listItem` changes signature to carry a checkbox state.

### Conformance fixtures are a build task

The GFM specification is published only as text; there is no fixture JSON to fetch. Deriving it is a build
concern and lives in the build, as a Mill task that converts the vendored text and proves itself in the process:
it converts CommonMark's own specification text and requires the result to equal the fixture file CommonMark
publishes, entry for entry, so fixtures cannot be produced by a converter that has drifted. Vendored content is
upstream input only, each file byte-identical to what its publisher serves, and no derived file is committed.

This is also where the repository's rule against adding a language runtime for a job the build can do is first
applied; the reasoning is recorded in the root agent guidelines.

### Conformance reporting

A recorded baseline gains a list of divergences, each an example number with a reason. The measured set is the
examples minus the divergences, and the score is a ratio against that set with the divergences printed beneath
it. The nine emphasis entries carry theirs: matches no published CommonMark version; cmark-gfm's 2019 emphasis
behaviour, which live GitHub no longer exhibits.

Layered reporting needs no new data file. The harness compares markdown-and-HTML pairs across the loaded fixture
sets and reports what a profile shares with CommonMark beside what is specific to it, which is where the
duplication the suite carries becomes visible rather than costly.

## Rejected alternatives

**Versioned profiles.** A CommonMark 0.29 profile beneath a 0.31.2 profile, with GFM built on the older base, was
proposed as a way to eliminate duplication in the suite and to reach the nine emphasis examples honestly. The
measurements above defeat it on both counts: no released CommonMark produces GFM's answer, so no version layering
reaches those nine; and the releases conflict on two inputs out of 628 shared, both being a rule GFM has already
adopted, so there is no behaviour to layer. The organizing instinct was sound and was redirected into how the
score is reported.

**Defaulted algebra methods.** Giving each new `Compiler` method a fallback would keep external implementors
compiling. It would also let a writer ship silently wrong output, with the conformance oracle — which measures
one writer of three — as the only thing that might notice. The break is taken instead.

**Extension recognition as an AST transformer.** Attractive for extended autolinks, since GFM defines them as a
scan over already-parsed text, and reusable for GitHub's later post-processing. Rejected for specified syntax
because the CST is built from inline nodes' spans: a transformer applied after the fact leaves the CST calling a
link plain text, so the two trees disagree and every CST-based tool stays blind to it. The stage is kept for the
unspecified features it genuinely suits.

**Filtering disallowed HTML in the writers.** The natural reading, since the rule is about output. It would mean
one rule with three implementations and a conformance oracle checking one of them, and would put the profile into
writers that otherwise have no use for it.

**Properties on the extension enum.** Considered as an `MdMeta`-shaped typed map for per-extension attributes.
An enum case is a singleton shared across every profile, so a map on it is shared too; parameterizing the cases
instead would let a set hold two differently configured members of the same extension with no way left to ask
whether that extension is on. Configuration, when needed, belongs to the profile.

## Unresolved

**Whether GitHub's unspecified features become extensions here.** Footnotes, alerts, `$…$` math, mentions, issue
references, emoji and heading anchors are not in the GFM specification and are not measured by its suite. They
are the obvious next demand from the knowledge base and the desktop application, and the AST-to-AST stage is
named as their home, but nothing here commits to building it or to which of them are worth having.

**Whether the Markdown writer should be able to target a profile.** Writing a document out under a GFM profile
means escaping bare URLs so that reading it back does not invent a link. That is settled for this work. Whether
the writer should more generally take a profile and refuse to emit syntax the target dialect cannot read is not.

**What conformance to a stale specification should mean over time.** The nine divergences are recorded against
GFM 0.29-gfm. If GitHub republishes, some may disappear and others may appear. Nothing yet says how a
re-vendoring reconciles a divergence list, beyond the existing rule that changing a fixture file is a deliberate
act that must move the version, the totals and the recorded score together.
