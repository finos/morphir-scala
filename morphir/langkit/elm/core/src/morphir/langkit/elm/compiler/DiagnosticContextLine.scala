package morphir.langkit.elm.compiler

final case class DiagnosticContextLine(
    line: Int,
    text: String,
    isErrorLine: Boolean
) derives CanEqual
