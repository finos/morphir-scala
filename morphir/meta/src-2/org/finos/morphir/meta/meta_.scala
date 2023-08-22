// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta

import scala.language.experimental.macros

private[meta] trait MetaCompat {
  this: Meta.type =>

  implicit def gen[A]: Meta[A] = macro MetaMacros.gen[A]
}

private[meta] object MetaMacros {
  import scala.reflect.macros.blackbox.Context

  def gen[A: c.WeakTypeTag](c: Context): c.Expr[Meta[A]] = {
    import c.universe._

    val A = c.weakTypeOf[A]
    if (!A.typeSymbol.isClass)
      c.abort(c.enclosingPosition, s"Type ${A.typeSymbol} is not a class")

    val sym         = A.typeSymbol.asClass
    val name        = sym.name.decodedName.toString
    val annotations = sym.annotations.map(a => c.untypecheck(a.tree))

    val fieldNames: Array[String] =
      if (!sym.isCaseClass) Array()
      else {
        sym.primaryConstructor
          .asMethod
          .typeSignature
          .paramLists
          .flatten
          .map(_.name.decodedName.toString.trim)
          .toArray
      }

    val fieldAnnotations: Array[List[c.Tree]] =
      if (!sym.isCaseClass) Array()
      else {
        sym.primaryConstructor
          .asMethod
          .typeSignature
          .paramLists
          .flatten
          .map(_.annotations.map(a => c.untypecheck(a.tree)))
          .toArray
      }

    c.Expr[Meta[A]](
      q"""new _root_.org.finos.morphir.meta.Meta[$A] {
          override def name = ${name}
          override def annotations = ${annotations}
          override def fieldNames = ${fieldNames}
          override def fieldAnnotations = ${fieldAnnotations}
        }"""
    )
  }
}
