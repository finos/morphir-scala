package org.finos.morphir.mill.toolchain

import scala.quoted.*

object StorageSizeInterpolator {
  def expand(context: Expr[StringContext], arguments: Expr[Seq[Any]])(using quotes: Quotes): Expr[StorageSize] = {
    import quotes.reflect.report

    val parts = context.valueOrAbort.parts
    arguments match {
      case Varargs(values) if values.nonEmpty =>
        report.errorAndAbort("storageSize does not accept interpolation", arguments)
      case Varargs(_) if parts.size != 1 =>
        report.errorAndAbort("storageSize does not accept interpolation", context)
      case Varargs(_) => ()
      case _          =>
        report.errorAndAbort("storageSize requires literal arguments; dynamic varargs are not supported", arguments)
    }

    val input = parts.head
    StorageSize.parse(input) match {
      case Left(error) => report.errorAndAbort(error.getMessage, context)
      case Right(size) =>
        val bytes = size.toBytes
        '{ ${ Expr(bytes) }.asInstanceOf[StorageSize] }
    }
  }
}
