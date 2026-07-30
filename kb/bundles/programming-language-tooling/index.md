---
okf_version: "0.2"
title: Programming Language Tooling
description: "Evidence-backed references and tutorials for syntax trees, traversal, interoperability, transformation pipelines, and toolchain design."
---

# Programming Language Tooling

Evidence-backed references and tutorials for syntax trees, traversal, interoperability, transformation pipelines, and toolchain design.

## Orientation

This bundle separates general mechanisms from Morphir-specific conclusions. Each concept is independently useful;
the sequence below forms a Scala-first tutorial:

1. [Syntax trees and intermediate representations](/syntax-trees-and-intermediate-representations.md)
2. [Tree traversal, visitors, cursors, and rewriting](/tree-traversal-visitors-cursors-and-rewriting.md)
3. [Structural tree interoperability](/structural-tree-interoperability.md)
4. [Node identity and addressability](/node-identity-and-addressability.md)
5. [Attribution of typed trees](/attribution-of-typed-trees.md)
6. [RDF, linked data, and provenance](/rdf-linked-data-and-provenance.md)
7. [Transformation pipelines](/transformation-pipelines.md)
8. [Guidance for a Morphir toolchain](/morphir-toolchain-guidance.md)
9. [Morphir attribution evolution](/morphir-attribution-evolution.md)
10. [Typed attribution guidance for morphir-scala](/typed-attribution-guidance-for-morphir-scala.md)

The [Scala 3 and Kyo implementation notes](/scala-3-and-kyo-implementation-notes.md) are a versioned companion rather
than a prerequisite. Main concepts use short callouts only where those implementation features materially help.

### Find a mechanism by problem

| Problem | Start here |
| --- | --- |
| Preserve exact source form | [Syntax trees and intermediate representations](/syntax-trees-and-intermediate-representations.md) |
| Analyze every node exhaustively | [Tree traversal, visitors, cursors, and rewriting](/tree-traversal-visitors-cursors-and-rewriting.md) |
| Navigate to parents or siblings | [Tree traversal, visitors, cursors, and rewriting](/tree-traversal-visitors-cursors-and-rewriting.md#cursors-and-zippers) |
| Run one query over several tree models | [Structural tree interoperability](/structural-tree-interoperability.md) |
| Identify nodes within a snapshot | [Node identity and addressability](/node-identity-and-addressability.md) |
| Relate nodes across rewrites | [Node identity and addressability](/node-identity-and-addressability.md#continuity-is-an-asserted-relation) |
| Attach and query typed facts | [Attribution of typed trees](/attribution-of-typed-trees.md) |
| Support user-controlled external decorators | [Morphir attribution evolution](/morphir-attribution-evolution.md#morphir-elm-external-decorators) |
| Exchange linked data or record provenance | [RDF, linked data, and provenance](/rdf-linked-data-and-provenance.md) |
| Connect parsing, transformation, and generation | [Transformation pipelines](/transformation-pipelines.md) |
| Evaluate a Morphir architecture | [Guidance for a Morphir toolchain](/morphir-toolchain-guidance.md) |
| Evaluate Morphir attribution strategies | [Typed attribution guidance for morphir-scala](/typed-attribution-guidance-for-morphir-scala.md) |
| Check Scala 3 or Kyo implementation value | [Scala 3 and Kyo implementation notes](/scala-3-and-kyo-implementation-notes.md) |

## Foundations

* [Syntax trees and intermediate representations](/syntax-trees-and-intermediate-representations.md) - Distinguish parse trees, concrete and abstract syntax trees, semantic models, and intermediate representations by the information each preserves.
* [Tree traversal, visitors, cursors, and rewriting](/tree-traversal-visitors-cursors-and-rewriting.md) - Compare recursive traversal, folds, typed visitors, cursors, zippers, and immutable rewriting by their observable navigation and transformation properties.
* [Structural tree interoperability](/structural-tree-interoperability.md) - Explain how minimal structural protocols and explicit projections enable generic tooling without replacing typed tree models.
* [Node identity and addressability](/node-identity-and-addressability.md) - Compare semantic names, snapshot-scoped identities, structural paths, source locations, hashes, and derivation across tree rewrites.
* [Attribution of typed trees](/attribution-of-typed-trees.md) - Explain intrinsic fields, typed payloads, side tables, overlays, relation graphs, and preservation policies for tree attribution.
* [RDF, linked data, and provenance](/rdf-linked-data-and-provenance.md) - Relate RDF triples, datasets, named graphs, statement qualification, PROV, and SHACL to typed-tree attribution.
* [Transformation pipelines](/transformation-pipelines.md) - Explain typed phase composition, processor lifecycles, task graphs, diagnostics, incremental work, and frontend-to-backend generation.

## Synthesis

* [Guidance for a Morphir toolchain](/morphir-toolchain-guidance.md) - Derive contextual Morphir toolchain guidance from observed tree, traversal, interoperability, and pipeline capabilities.
* [Morphir attribution evolution](/morphir-attribution-evolution.md) - Trace generic IR attributes, Morphir-Elm external decorators, and the v4 draft's explicit and layered attribution designs.
* [Typed attribution guidance for morphir-scala](/typed-attribution-guidance-for-morphir-scala.md) - Rank attribution strategies for morphir-scala while preserving typed IR semantics, core stability, local efficiency, and open interchange.

## Implementation companion

* [Scala 3 and Kyo implementation notes](/scala-3-and-kyo-implementation-notes.md) - Record versioned Scala 3 and Kyo implementation techniques that materially support typed language-tooling designs.
