package morphir.langkit.elm.parser

import morphir.langkit.core.{Reported, Severity, Span}
import morphir.langkit.elm.Leniency
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.cst.CstVisitor.children

/**
 * Elm's tuple arity limit: two entries or three, never more.
 *
 * The grammar has nothing to say about this — `( 1, 2, 3, 4 )` is a perfectly well-formed tuple as far as the parser is
 * concerned, which is why `elm/compiler` also catches it after parsing, during canonicalisation. Checking it here
 * rather than in a production keeps the rule where the error message can be a sentence rather than an expectation set,
 * and lets a module with four oversized tuples report all four.
 *
 * The limit applies wherever a tuple can be written: as an expression, as a type, and as a pattern.
 */
object TupleArityChecker:

  /** The largest tuple Elm accepts. */
  val maximumEntries: Int = 3

  /** Every oversized tuple in `module`, in source order. */
  def check(module: CstModule, source: String, leniency: Leniency): List[Reported[ParseDiagnostic]] =
    val severity = if leniency == Leniency.Reject then Severity.Error else Severity.Advisory
    oversized(module).map { case (span, entries) =>
      Reported(ParseDiagnostic.tupleTooLarge(source, span, entries), severity)
    }

  private def oversized(node: CstNode): List[(Span, Int)] =
    val here = node match
      case n: CstTupleLiteral if n.elements.size > maximumEntries => List(n.span -> n.elements.size)
      case n: CstTupleType if n.elements.size > maximumEntries    => List(n.span -> n.elements.size)
      case n: CstTuplePattern if n.elements.size > maximumEntries => List(n.span -> n.elements.size)
      case _                                                      => Nil
    here ::: children(node).flatMap(oversized)
