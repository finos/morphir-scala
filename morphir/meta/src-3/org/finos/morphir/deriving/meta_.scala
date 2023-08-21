// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta

import scala.annotation._
import scala.quoted._

private[meta] trait MetaCompat {
  this: Meta.type =>

  implicit inline def gen[A]: Meta[A] = ${ MetaMacros.gen[A] }
}

private[meta] object MetaMacros {

  def gen[A: Type](using Quotes): Expr[Meta[A]] = {
    import quotes.reflect._

    val A = TypeRepr.of[A]
    if (!A.typeSymbol.isClassDef)
      report.errorAndAbort(s"Type ${A.typeSymbol} is not a class")

    val nameExpr = Expr(A.show.split("[.]").last)

    def anns(s: Symbol): Expr[List[Annotation]] = Expr.ofList(s.annotations.map(_.asExpr).collect {
      case '{ $a: Annotation } => a
    }.filter {
      case '{ $a: internal.Child[_] } => false
      case _                          => true
    })
    val annotationsExpr      = anns(A.typeSymbol)
    val fieldNamesExpr       = Expr(A.typeSymbol.primaryConstructor.paramSymss.flatten.map(_.name.trim))
    val fieldAnnotationsExpr = Expr.ofList(A.typeSymbol.primaryConstructor.paramSymss.flatten.map(anns(_)))

    '{
      new Meta[A] {
        override def name             = ${ nameExpr }
        override def annotations      = ${ annotationsExpr }
        override def fieldNames       = ${ fieldNamesExpr }.toArray
        override def fieldAnnotations = ${ fieldAnnotationsExpr }.toArray
      }
    }
  }
}
