package org.finos.morphir.mill

final case class SourceLocation(file: String, line: Int, enclosing: String) {
  def render: String = s"$file:$line ($enclosing)"
}

object SourceLocation {
  given generated(using
      file: sourcecode.File,
      line: sourcecode.Line,
      enclosing: sourcecode.FullName
  ): SourceLocation = SourceLocation(file.value, line.value, enclosing.value)

  private[mill] val serialized: SourceLocation =
    SourceLocation("<serialized ModuleId>", 0, "upickle.default.ReadWriter")
}
