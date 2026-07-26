package morphir.langkit.trees

/**
 * Generic contract for tree-shaped data that participates in the query DSL.
 *
 * An instance describes how to navigate values of `T`: what kind of node it is, its children in traversal order,
 * optional named sub-trees ("fields"), and optional leaf text.
 *
 * v1 is intentionally string-based; compile-time type narrowing and typeclass derivation are left to a later revision.
 */
trait QueryableTree[T]:
  /** Human- and query-readable name of the node's kind. */
  def nodeType(t: T): NodeTypeName

  /** Every child of `t` in a stable traversal order. */
  def children(t: T): Seq[T]

  /** Named sub-trees keyed by field name. Values must be a (possibly empty) subset of `children(t)`. */
  def fields(t: T): Map[FieldName, Seq[T]]

  /** Raw text for leaf nodes; `None` for compound nodes. */
  def text(t: T): Option[String]

object QueryableTree:
  inline def apply[T](using qt: QueryableTree[T]): QueryableTree[T] = qt
