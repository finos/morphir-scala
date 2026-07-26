package morphir.langkit.elm.compiler.abi

import scala.util.control.NonFatal

import morphir.langkit.elm.compiler.CompilerComponent
import morphir.langkit.elm.compiler.Krueger

object InvokeCompiler:

  import InvokeJson.decode
  import InvokeJson.encode
  import InvokeJson.given

  private val compiler = Krueger.defaultCompiler

  def invoke(op: String, inputJson: String): String =
    encode(dispatch(op, inputJson))

  private def dispatch(op: String, inputJson: String): InvokeResponse =
    try
      InvokeOp.fromWireName(op) match
        case Some(InvokeOp.ParseCst) =>
          val request = decode[SourceRequest](inputJson)
          val result  = CompilerComponent.runUnit(compiler.parseCst(request.source))
          InvokeResponse.fromCompileResult(result, _.toString)
        case _ =>
          unknownOperation(op)
    catch
      case NonFatal(error) =>
        InvokeResponse.failure(
          errors = Vector(
            InvokeError(
              phase = "internal",
              message = messageOf(error)
            )
          )
        )

  private def unknownOperation(op: String): InvokeResponse =
    InvokeResponse.failure(
      errors = Vector(
        InvokeError(
          phase = "internal",
          message = s"unknown operation: $op"
        )
      )
    )

  private def messageOf(error: Throwable): String =
    Option(error.getMessage)
      .map(_.trim)
      .filter(_.nonEmpty)
      .getOrElse(error.getClass.getSimpleName)
