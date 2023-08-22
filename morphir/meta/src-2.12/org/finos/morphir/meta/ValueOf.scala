// This file was taken from/originally ispired by the shapely project: https://gitlab.com/fommil/shapely
// backport of ValueOf
package scala

import scala.language.experimental.macros

@scala.annotation.implicitNotFound(msg = "No singleton value available for ${T}.")
final class ValueOf[T](val value: T) extends AnyVal
object ValueOf {
  implicit def gen[A]: ValueOf[A] = macro ValueOfMacros.gen[A]
}

private[scala] object ValueOfMacros {
  import scala.reflect.macros.whitebox.Context

  def gen[A: c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val A      = c.weakTypeOf[A]
    val result = q"new _root_.scala.ValueOf[$A](${A.termSymbol})"
    // println(result)
    // println(scala.util.Try(c.typecheck(result)))
    c.Expr[A](result)
  }
}
