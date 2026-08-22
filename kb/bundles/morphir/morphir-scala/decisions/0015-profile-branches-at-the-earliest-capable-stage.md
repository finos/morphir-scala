---
type: Decision Record
title: "Profile-dependent syntax branches at the earliest stage whose tree can record it"
description: "A parse profile changes behaviour at the earliest pipeline stage whose output tree can record the decision, and no later, so the concrete and abstract syntax trees never disagree."
state: Accepted
decided: 2026-08-22
tags: [langkit, markdown, parser, cst, ast, profile, pipeline]
status: stable
---

# 0015 — Profile-dependent syntax branches at the earliest stage whose tree can record it

When an [`MdProfile`](../../../intent/0035-github-flavored-markdown-profile.md) changes what a parse recognizes,
the branch goes at the **earliest pipeline stage whose output tree can record the decision, and no later**.

A stage receives the profile only if it is the earliest capable stage for some decision. In the Markdown langkit
that is block parsing, inline parsing, and lowering. Neither the concrete-syntax-tree rewrite stage nor the
abstract-syntax-tree rewrite stage takes a profile, because anything they could decide an earlier stage could have
recorded.

| Stage | Records a decision by | Profile |
| --- | --- | --- |
| Block parse | emitting a CST fragment kind | yes |
| Inline parse | emitting a phrasing node with a span, which the CST then tiles | yes |
| CST to CST | rewriting nodes; source spans go stale | no |
| CST to AST | producing a different AST node for the same CST | yes |
| AST to AST | rewriting the AST; the CST is left behind | no |

## Why

The langkit publishes two trees that must agree. The concrete syntax tree records what was written, keeps every
token and its source span, and prints back byte-for-byte. The abstract syntax tree records what it means.
`MD.parser.parse` produces the second directly; `MD.cst.parse` followed by `MD.cst.lower` produces it by way of the
first, and the two paths must arrive at the same tree. `CstRoundTripTests` is what enforces that.

The corollary that gives the rule its name came from frontmatter, which was the first profile-dependent construct.
Lowering handles a frontmatter node with no idea whether frontmatter was enabled — the concrete syntax tree already
answered by having the node or not. Generalized: **a profile decision made at an earlier stage is recorded in the
tree it emits, so later stages need no profile.** A stage needs the profile exactly when it is the first stage that
could record the answer.

Branching later than that stage is what breaks the agreement, and it breaks it silently. The worked case is GitHub's
extended autolinks, which its specification defines as a scan over already-parsed text — the shape that most invites
an abstract-syntax-tree rewrite. Written that way, `MD.parser.parse` yields a link where `MD.cst.lower` yields plain
text. Applying the same rewrite to both paths repairs the two abstract trees and leaves the concrete one still
calling the URL a run of text, so `MD.cst.print` is unaffected but every concrete-tree consumer — the tree query DSL
in `morphir-langkit-markdown-trees` among them — stays blind to a link the renderer displays. Recognizing it during
inline parsing instead makes both trees see it at once, because the concrete tree is materialized *from* the inline
nodes' spans.

The rule also answers where a construct with no tree-shape change belongs. GitHub's disallowed-raw-HTML filter parses
byte-identically to a plain parse and differs only in what the tag *means*, so its earliest capable stage is lowering
— which is precisely the stage where source form becomes meaning. That keeps the concrete tree verbatim, so
round-tripping is unaffected, and keeps the profile out of the three writers, which would otherwise each need their
own copy of one rule.

## Alternatives rejected

**Thread the profile through every stage.** Cheaper to write and superficially more flexible. Rejected because a
profile parameter on a stage that decides nothing is a claim that behaviour might vary there, and the next
contributor will make it vary — which is the late branch this record exists to prevent. Absence of the parameter is
the enforcement.

**Recognize every extension in one place, after parsing.** A single rewrite over the abstract tree is the smallest
possible change and needs no new fragment kinds. It is right for features that are genuinely post-processing and
wrong for specified syntax, for the reason above: the concrete tree never learns about the construct. The
distinction that decides it is whether the dialect's specification calls the construct syntax. GitHub's own
post-rendering features — mentions, issue references, emoji, heading anchors — are not, and the abstract-tree stage
is the right home for them when they are built.

**Let the writers branch on the profile.** Considered for the raw-HTML filter, since the rule is about output.
Rejected because it turns one rule into three implementations across `morphir-langkit-markdown-scalatags`,
`morphir-langkit-markdown-kyo-ui` and the Markdown writer, with the conformance oracle measuring only one of them.

## Consequences

`InlineParser.parse` and `Lower.lower` take the profile; `MD.cst.lower` gains a `using` clause and stays
source-compatible through `MdProfile.given default`. The two rewrite stages are named in the pipeline and left
unbuilt.

New extension work must state which stage it branches at and why that stage is the earliest capable one. A proposal
that reaches for a later stage is either wrong or is telling you the construct is not syntax.

Nothing here constrains features outside a dialect's specification. Those give up concrete-tree fidelity knowingly,
which is a different trade from giving it up by accident.

## Revisit when

The langkit gains a third tree, or the concrete tree stops being materialized from inline nodes' spans. Both would
change which stage is capable of recording what, which is the only thing this rule depends on.
