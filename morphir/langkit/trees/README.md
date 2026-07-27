# morphir-langkit-trees

A tree-sitter-inspired query DSL, generic over any tree with a `QueryableTree[T]` instance.

## Why

Hand-written assertions like "find the first `CstValueDeclaration`, then check its body is a `CstIntLiteral` with
value 42" are verbose and tightly coupled to the tree shape. The [tree-sitter query language][ts-queries] solves the
same problem with compact S-expression patterns and captures. This module brings that style to Morphir's langkits —
and to any third-party tree that provides an instance of the typeclass.

[ts-queries]: https://tree-sitter.github.io/tree-sitter/using-parsers#pattern-matching-with-queries

## The typeclass

```scala
trait QueryableTree[T]:
  def nodeType(t: T): NodeTypeName
  def children(t: T): Seq[T]
  def fields(t: T): Map[FieldName, Seq[T]]
  def text(t: T): Option[String]
```

The four methods describe: the kind of node, its children in traversal order, its named sub-trees (so patterns can
navigate by field name), and optional leaf text. `NodeTypeName` and `FieldName` are validated newtypes — non-empty,
non-blank, identifier-shaped. The Elm langkit ships instances for both its CST and AST; third parties write one for
their own tree.

## Query syntax

| Form | Meaning |
| --- | --- |
| `(NodeType)` | Match any node whose `nodeType` equals `NodeType` |
| `(NodeType field: (Child))` | Match a node, and a named sub-tree within it |
| `@name` | Capture the matched node as `name` |
| `_` or `(_)` | Wildcard — match any node |
| `[(A) (B)]` | Alternation — match either `(A)` or `(B)` |
| `(Parent (A)?)` | Optional child — zero or one occurrence |
| `(Parent (A)*)` | Zero-or-more children |
| `(Parent (A)+)` | One-or-more children |
| `(Parent (A) . (B))` | Anchor: `(A)` and `(B)` must match adjacent children |
| `(Parent !field)` | Negated field: named field must be absent or empty |
| `(NodeType) (NodeType2)` | Multi-pattern: two independent patterns in one query |
| `(#eq? @a @b)` | Predicate: captured texts must be equal |
| `(#eq? @a "literal")` | Predicate: captured text equals a literal |
| `(#match? @a "regex")` | Predicate: captured text matches the regex |
| `(#not-eq? @a "literal")` | Predicate: captured text differs from a literal |
| `(#not-match? @a "regex")` | Predicate: captured text does not match the regex |
| `;; line comment` | Ignored through to end of line |

Anchor support is limited to `.` between two unfielded child patterns; other placements fail with
`invalid anchor placement`. Directives such as `#set!` are unsupported and fail with an explicit
`Unsupported directive: ...` parse diagnostic.

## Example

```scala
import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.CstName
import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.query.*
import parsley.Success

val Success(module) = Elm.parseCst("""
module M exposing (..)

main = 42
""".stripMargin): @unchecked

val Success(query) = QueryParser.parse("(CstValueDeclaration name: (CstName) @n)"): @unchecked

val names = Matcher
  .matches(query, module)
  .flatMap(m => CaptureName.make("n").toOption.flatMap(cn => m.captures.get(cn)))
  .collect { case n: CstName => n.value }
  .toList
// names == List("main")
```

This example is backed by
[`CstQueryableTreeSpec`](../elm/core/test/src/morphir/langkit/elm/cst/CstQueryableTreeSpec.scala).

## Rendering queries

Two APIs produce canonical S-expression output from a parsed `Query`, and both round-trip — `parse -> render -> parse`
preserves meaning and normalises whitespace:

- **`QueryPretty.render`** — backed by Kindlings `FastShowPretty`, with `RenderConfig.Compact` (one clause per line)
  and `RenderConfig.Normal` (indented).
- **`QueryPrinter.print`** — a lightweight, dependency-free alternative producing a single compact line.

## Traversal

- `QueryVisitor` gives typed dispatch over `Query`, `Pattern`, `FieldPattern`, `Predicate`, and `PredicateArg`, plus
  `foldLeft`/`count`, `collect`/`collectPostOrder`, and an effectful `traverse`.
- `QueryCursor` gives zipper-style navigation over `QueryNode`: `firstChild`, `lastChild`, `nextSibling`,
  `previousSibling`, `parent`, `depth`, `isRoot`, `isLeaf`, `root`, and a deterministic `preOrder`.

## Kyo-backed execution

`QueryLogic` defines `QueryEffect[Ctx, Log, Err, A]`, tracking `Var` (context and soft errors), `Emit` (logs), and
`Abort` (fail-fast). Context threading goes through `readContext`/`setContext`/`updateContext`; `log` accumulates
messages; `error` gathers and `failFast` aborts. `run(initialContext)` returns context, logs, gathered errors, and the
final value.

`QueryExecutionPipeline` stages execution as `normalize` → `analyze` → `validate` → `lower` → `execute`, each emitting
deterministic logs and returning typed intermediates (`Analysis`, `Plan`, `Lowered`), so planner work can insert
phases without changing caller entry points.

## Custom predicates

Predicates resolve through a `PredicateRegistry`. The default ships `#eq?`, `#match?`, `#not-eq?`, and `#not-match?`,
all backed by `QueryableTree[T].text`. Callers register their own per query without touching the matcher:

```scala
val registry = PredicateRegistry.default.withPredicate(
  PredicateName.make("#my-pred?").toOption.get,
  new PredicateImpl:
    def evaluate[T](args: PredicateArgs, captures: Map[CaptureName, T])(using
        qt: QueryableTree[T]
    ): Boolean = ???
)
Matcher.matches(query, root, registry)
```

## Artifact

`org.finos.morphir::morphir-langkit-trees` — JVM, Scala.js, and Scala Native.
