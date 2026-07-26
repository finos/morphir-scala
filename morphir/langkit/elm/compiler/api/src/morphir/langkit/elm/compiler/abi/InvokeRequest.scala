package morphir.langkit.elm.compiler.abi

final case class SourceRequest(source: String) derives CanEqual

final case class PrettyQueryRequest(query: String) derives CanEqual

final case class RunQueryRequest(query: String, rootJson: String, treeKind: String) derives CanEqual
