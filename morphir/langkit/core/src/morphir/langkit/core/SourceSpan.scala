package morphir.langkit.core

/** A diagnostic's source location: a `[start, end)` character range plus the 1-based line/column of `start`. */
final case class SourceSpan(start: Int, end: Int, line: Int, column: Int) derives CanEqual
