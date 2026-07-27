package morphir.langkit.elm.compiler.abi

final case class SourceRequest(source: String) derives CanEqual

final case class QueryRequest(query: String) derives CanEqual

/**
 * Request for [[InvokeOp.RunQuery]]: parse `source` into the tree named by `treeKind` (`"cst"` or `"ast"`), then run
 * `query` against it.
 *
 * The tree is named by source rather than sent pre-serialized: nothing in the langkit deserializes a CST or AST from
 * JSON, so the earlier `rootJson` field could never have been honoured.
 */
final case class RunQueryRequest(query: String, source: String, treeKind: String) derives CanEqual
