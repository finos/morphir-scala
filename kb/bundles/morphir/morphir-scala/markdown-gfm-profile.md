---
type: Capability
title: Markdown GFM Profile
description: "morphir-langkit-markdown parses CommonMark 0.31.2 and, behind MdProfile.gfm, GitHub Flavored Markdown's five extensions, with a conformance-measured writer."
tags: [langkit, markdown, gfm, commonmark, conformance]
status: stable
---

# Markdown GFM Profile

`morphir-langkit-markdown` recognizes two dialects. `MdProfile.commonmark` is the base grammar alone.
`MdProfile.gfm` adds the five extensions GitHub Flavored Markdown defines, each gated on `MdProfile.extensions`
rather than folded into the base grammar, so a caller can enable one extension without the rest.

## The five extensions

| Extension | Recognizes | Branches at |
| --- | --- | --- |
| `Tables` | Pipe tables: a header row, a delimiter row fixing per-column alignment, and body rows | Block parse |
| `TaskListItems` | `[ ]` and `[x]` at the start of a list item's first paragraph, rendered as a disabled checkbox | Block parse |
| `Strikethrough` | `~~struck~~`, rendered as `del` | Inline parse |
| `Autolinks` | Bare URLs, `www.` hostnames and email addresses linked without `<>` around them | Inline parse |
| `TagFilter` | Escapes a fixed list of raw HTML tags (`script`, `style`, `iframe` and kin) rather than passing them through | Lowering (CST to AST) |

"Branches at" names the earliest pipeline stage whose output tree can record the extension's decision, and no
later — the operating rule for every profile-dependent construct in this module. Tables and task list items are
block constructs, decided where the block parser emits a CST fragment kind. Strikethrough is a delimiter run on
the same machinery emphasis uses, and extended autolinks are a scan over already-produced phrasing content; both
are decided during inline parsing, which is also where the concrete syntax tree is materialized from. The tag
filter is the one construct with no tree-shape change at all — the parse is byte-identical whether or not the
filter is on, and only the meaning of the tag differs — so it branches at lowering, where source form becomes
meaning. Neither the CST-to-CST rewrite stage nor the AST-to-AST rewrite stage ever takes a profile: anything
either could decide, an earlier stage already could have. Full reasoning:
[decision 0015](/decisions/0015-profile-branches-at-the-earliest-capable-stage.md).

## Conformance

Both dialects are measured byte-exact against their own published example set, through the ScalaTags writer as
conformance oracle — a comparison with no canonicalization, so an extra or missing byte fails the same as a wrong
tree would.

- **CommonMark 0.31.2**: 652 of 652.
- **GitHub Flavored Markdown 0.29-gfm**: 663 of 663 measured examples.

Each GFM example is scored against the base grammar plus the one extension its own fence names, never the full
GFM profile — an example about tables makes no claim about strikethrough or the tag filter, so measuring it under
every extension at once would credit or blame it for behavior it never claimed.

Nine of the specification's own examples are recorded as divergences rather than targets: all nine concern nested
strong emphasis, where GFM 0.29-gfm's text — a snapshot of cmark-gfm from April 2019 — expects a reading no
released CommonMark version, including the one GFM's own base was drawn from, ever produced. Matching them would
mean regressing the CommonMark profile to satisfy a stale document.

## The writer's guarantee

The Markdown writer carries the same round-trip guarantee GFM's conformance measurement does. Every measured GFM
example — scoped extension by extension exactly as conformance scores it, the nine divergences excluded —
survives being written back out to Markdown text and reparsed with an identical render, the same
guarantee the CommonMark corpus already held. A table, a task list item and strikethrough all round-trip through
the writer, reparsed always under the full GFM profile: the writer spells defensively for the widest dialect that
might read a document back, escaping any punctuation run that dialect could otherwise misread regardless of
whether the profile that produced the tree enabled the extension in question.

A tag-filtered node is different, and the writer does not guess there. Filtering and plain authorship can produce
the identical string — a filtered `<script>` and an author who typed `&lt;script>` outright both end up as the same
bytes — so nothing about the value itself says which one happened. The decision is recorded instead, in the tree,
the same as every other profile-dependent decision this parser makes: lowering attaches the pre-filter original
to a filtered node's data, and the writer emits that recorded original back rather than inferring one from the
string's shape.
