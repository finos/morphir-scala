package morphir.langkit.core.scanner

import scala.quoted.*

/**
 * Backs [[ScanPhase.apply]]. Validates the literal argument at compile time and aborts compilation for a non-constant
 * or blank argument, pointing the caller at [[ScanPhase.fromString]] for dynamic values.
 */
private[scanner] object ScanPhaseMacro:
  def applyImpl(value: Expr[String])(using Quotes): Expr[ScanPhase] =
    import quotes.reflect.report
    value.value match
      case Some(literal) if literal.isBlank =>
        report.errorAndAbort(
          s"scan phase must be non-empty and non-blank: '$literal'; use ScanPhase.fromString for a dynamic value"
        )
      case Some(_) =>
        '{ ScanPhase.unsafe($value) }
      case None =>
        report.errorAndAbort("scan phase must be a literal; use ScanPhase.fromString for a dynamic value")
