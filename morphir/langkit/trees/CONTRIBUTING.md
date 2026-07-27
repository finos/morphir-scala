# Contributing to `morphir-langkit-trees`

The namespace guide [`morphir/langkit/CONTRIBUTING.md`](../CONTRIBUTING.md) governs here, along with the root
[CONTRIBUTING.md](../../../CONTRIBUTING.md) and [AGENTS.md](../../../AGENTS.md). This file carries only what is true of
`langkit-trees`.

## The instance invariant

A `QueryableTree[T]` instance must hold to one rule the matcher relies on and does not check:

> every value in `fields(t)` must also appear in `children(t)`

Field patterns narrow a search that has already been scoped by `children`. An instance that exposes a field its
`children` omits produces a pattern that silently never matches — no error, no diagnostic, just an empty result.

Two more conventions instances follow:

- **`nodeType` is the raw case-class simple name** (`"CstIntLiteral"`, not `"cst-int-literal"`). Query text names these
  directly, so renaming a case class is a breaking change to every query mentioning it.
- **`children` order must be stable**, because anchors (`.`) and positional quantifiers are defined in terms of it.

[`ToyTree`](./test/src/morphir/langkit/trees/ToyTree.scala) is the reference implementation and the fixture the
typeclass's own tests run against. It is the cheapest place to reproduce a matcher bug — start there before reaching
for a real CST.

## Adding an instance for your own tree

1. Pick a `nodeType` convention and keep it uniform across the tree.
2. Implement `children` in a stable traversal order.
3. Decide which constructor parameters become named `fields`, holding to the subset invariant above.
4. Implement `text` for leaf-like nodes whose primary content is a scalar — string, number, identifier. Return `None`
   for compound nodes; the default predicates (`#eq?`, `#match?`, and their negations) are all defined in terms of
   `text`, so a node with no text is invisible to them.

## v1 is string-based on purpose

`NodeTypeName`, `FieldName`, `CaptureName`, and `PredicateName` are validated newtypes over `String` (via
[neotype](https://github.com/kitlangton/neotype)), and `RegexPattern` pre-compiles at construction. Captures are
returned as `Map[CaptureName, T]` — untyped in the node's own type.

Compile-time narrowing is deliberately deferred: a `NodeTypes` type member on the typeclass, Mirror-backed derivation,
and a match type for typed captures are all sketched but not built. Adding validation to the newtypes is welcome;
introducing a parallel typed API alongside the string one is a larger design change worth raising first.

## Changing query syntax

The syntax table in the [README](./README.md) is the specification, and the BDD scenarios in
[`query.feature`](../itest/resources/features/query.feature) are the executable form of it. A syntax change means
updating all three: parser, table, and feature file.

Both renderers must keep round-tripping — `parse -> render -> parse` has to preserve meaning for any query the parser
accepts, for `QueryPretty` and `QueryPrinter` alike. New syntax needs a rendering case in both, and the round-trip is
worth asserting directly rather than only through a matcher result.

Unsupported input should fail with an explicit diagnostic rather than a silent non-match — the existing
`Unsupported directive: ...` and `invalid anchor placement` messages are the pattern to follow.
