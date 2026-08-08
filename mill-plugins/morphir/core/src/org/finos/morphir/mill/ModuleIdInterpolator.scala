package org.finos.morphir.mill

import scala.quoted.*

object ModuleIdInterpolator {
  def expand(context: Expr[StringContext], arguments: Expr[Seq[Any]])(using quotes: Quotes): Expr[ModuleId] = {
    import quotes.reflect.*

    val parts = context.valueOrAbort.parts
    arguments match {
      case Varargs(values) if values.nonEmpty =>
        report.errorAndAbort("moduleId does not accept interpolation", arguments)
      case Varargs(_) if parts.size != 1 =>
        report.errorAndAbort("moduleId does not accept interpolation", context)
      case Varargs(_) => ()
      case _          =>
        report.errorAndAbort("moduleId requires a literal; dynamic varargs are not supported", arguments)
    }

    val value    = parts.head
    val position = Position.ofMacroExpansion
    val location = SourceLocation(
      position.sourceFile.path,
      position.startLine + 1,
      Symbol.spliceOwner.fullName
    )
    ModuleId.validate(value, location) match {
      case Left(error) => report.errorAndAbort(error.getMessage, context)
      case Right(_)    => '{ ${ Expr(value) }.asInstanceOf[ModuleId] }
    }
  }
}
