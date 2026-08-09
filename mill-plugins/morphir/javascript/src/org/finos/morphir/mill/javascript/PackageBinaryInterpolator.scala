package org.finos.morphir.mill.javascript

import scala.quoted.*

object PackageBinaryInterpolator {
  def expand(context: Expr[StringContext], arguments: Expr[Seq[Any]])(using quotes: Quotes): Expr[PackageBinary] = {
    import quotes.reflect.*

    val parts = context.valueOrAbort.parts
    arguments match {
      case Varargs(values) if values.nonEmpty =>
        report.errorAndAbort("packageBinary does not accept interpolation", arguments)
      case Varargs(_) if parts.size != 1 =>
        report.errorAndAbort("packageBinary does not accept interpolation", context)
      case Varargs(_) => ()
    }

    val value    = parts.head
    val position = Position.ofMacroExpansion
    val location = PackageBinary.CallSite(
      position.sourceFile.path,
      position.startLine + 1,
      Symbol.spliceOwner.fullName
    )
    PackageBinary.validate(value, location) match {
      case Left(error) => report.errorAndAbort(error.getMessage, context)
      case Right(_)    =>
        '{
          given PackageBinary.CallSite = PackageBinary.CallSite(
            ${ Expr(location.file) },
            ${ Expr(location.line) },
            ${ Expr(location.enclosing) }
          )
          PackageBinary.parse(${ Expr(value) }).fold(throw _, identity)
        }
    }
  }
}
