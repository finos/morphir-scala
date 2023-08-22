// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
package org.finos.morphir.meta

import scala.language.experimental.macros

private[meta] trait ShapelyCompat {
  this: Shapely.type =>

  // ideally we'd like to have bounds on the B here, but Scala 2.12 and below doesn't like that
  implicit def genCaseClass[A <: Product with Serializable, B]: Shapely[A, B] = macro ShapelyMacro.genCaseClass[A, B]
  implicit def genSealedTrait[A, B]: Shapely[A, B] = macro ShapelyMacro.genSealedTrait[A, B]
}

private[meta] object ShapelyMacro {
  import scala.reflect.macros.whitebox.Context

  def genCaseClass[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[Shapely[A, B]] = {
    import c.universe._

    val A = c.weakTypeOf[A]

    if (!A.typeSymbol.isClass || !A.typeSymbol.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"Type ${A.typeSymbol} is not a case class")

    val fields = A.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.asMethod }.toList

    val result =
      if (fields.isEmpty) {
        val tcons = c.mirror.staticClass(s"_root_.org.finos.morphir.meta.CaseClass0")
        val B     = tq"$tcons[$A]"
        val cons =
          if (A.typeSymbol.isModuleClass) q"${A.termSymbol}"
          else q"${A.typeSymbol.companion}()"
        q"""new _root_.org.finos.morphir.meta.Shapely[$A, $B] {
                override def to(a: $A): $B = ${tcons.companion}()
                override def from(b: $B): $A = $cons
              }"""
      } else {
        val tcons      = c.mirror.staticClass(s"_root_.org.finos.morphir.meta.CaseClass${fields.length}")
        val tparams    = fields.map { m => m.typeSignatureIn(A).resultType }
        val B          = tq"$tcons[$A , ..$tparams]"
        val to_getters = fields.map { f => q"a.${f.name.toTermName}" }
        val from_getters = (1 to fields.length).map { i =>
          val getter = TermName(s"_$i")
          q"b.$getter"
        }

        q"""new _root_.org.finos.morphir.meta.Shapely[$A, $B] {
                override def to(a: $A): $B = ${tcons.companion}(..$to_getters)
                override def from(b: $B): $A = ${A.typeSymbol.companion}(..$from_getters)
              }"""
      }

    // println(result)
    // println(scala.util.Try(c.typecheck(result)))
    c.Expr[Shapely[A, B]](result)
  }

  def genSealedTrait[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context): c.Expr[Shapely[A, B]] = {
    import c.universe._

    val A   = c.weakTypeOf[A]
    val cls = A.typeSymbol.asClass

    if (!cls.isSealed)
      c.abort(c.enclosingPosition, s"${A.typeSymbol} is not a sealed trait")

    // ordering is ill-defined, we use source ordering
    val parts =
      cls.knownDirectSubclasses.toList
        .map(_.asClass)
        .sortBy(_.pos.start)
        .map { cl =>
          if (cl.isModuleClass)
            internal.thisType(cl)
          else {
            val t = cl.toType
            val args = t.typeArgs.map { a =>
              val sym = a.typeSymbol
              val tSym = A
                .find(_.typeSymbol.name == sym.name)
                .getOrElse(
                  c.abort(
                    c.enclosingPosition,
                    s"type parameters on case classes ($t[${t.typeArgs}]) are not supported unless they are on the sealed trait ($A)"
                  )
                )
              a.substituteTypes(List(sym), List(tSym))
            }
            appliedType(t, args)
          }
        }

    val sealedtrait_cls = c.mirror.staticClass(s"_root_.org.finos.morphir.meta.SealedTrait${parts.length}")
    val sealedtrait     = appliedType(sealedtrait_cls, A :: parts) // == B.typeSymbol.asClass

    val to_matchers = parts.zipWithIndex.map {
      case (tp, i) =>
        val cons = c.mirror.staticClass(s"_root_.org.finos.morphir.meta.SealedTrait._${i + 1}")
        cq"p : $tp => ${cons.companion}.apply(p)"
    }

    val from_matchers = parts.zipWithIndex.map {
      case (_, i) =>
        val uncons = c.mirror.staticClass(s"_root_.org.finos.morphir.meta.SealedTrait._${i + 1}")
        cq"${uncons.companion}(p) => p"
    }

    val result =
      q"""new _root_.org.finos.morphir.meta.Shapely[$A, $sealedtrait] {
              override def to(a: $A): $sealedtrait = a match { case ..$to_matchers }
              override def from(b: $sealedtrait): $A = b match { case ..$from_matchers }
            }"""

    // println(result)
    // println(scala.util.Try(c.typecheck(result)))
    c.Expr(result)
  }
}
