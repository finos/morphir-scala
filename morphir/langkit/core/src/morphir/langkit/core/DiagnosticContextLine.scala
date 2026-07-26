package morphir.langkit.core

final case class DiagnosticContextLine(
    line: Int,
    text: String,
    isErrorLine: Boolean
) derives CanEqual
