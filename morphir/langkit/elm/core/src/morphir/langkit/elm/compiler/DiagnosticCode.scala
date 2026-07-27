package morphir.langkit.elm.compiler

import neotype.*

/** Stable alphanumeric code assigned to a parse or tokenizer diagnostic. */
type DiagnosticCode = DiagnosticCode.Type

object DiagnosticCode extends Newtype[String]:

  private val Pattern = raw"ELM-[PT]\d{3}".r

  override inline def validate(input: String): Boolean | String =
    if Pattern.matches(input) then true
    else s"DiagnosticCode must match ELM-P### or ELM-T###, got: $input"

  given CanEqual[Type, Type] = CanEqual.derived

  val UnexpectedEndOfInput: DiagnosticCode         = unsafeMake("ELM-P001")
  val UnexpectedToken: DiagnosticCode              = unsafeMake("ELM-P002")
  val SpecialisedParseFailure: DiagnosticCode      = unsafeMake("ELM-P003")
  val ConflictingOperators: DiagnosticCode         = unsafeMake("ELM-P004")
  val UnknownOperator: DiagnosticCode              = unsafeMake("ELM-P005")
  val TokenizerUnexpectedCharacter: DiagnosticCode = unsafeMake("ELM-T001")

  def isTokenizer(code: DiagnosticCode): Boolean =
    unwrap(code).startsWith("ELM-T")

  def kindLabel(code: DiagnosticCode): String =
    if isTokenizer(code) then "TOKENIZE ERROR"
    else "PARSE ERROR"
