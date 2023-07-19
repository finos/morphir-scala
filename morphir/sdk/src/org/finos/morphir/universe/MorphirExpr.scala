package org.finos.morphir.universe

sealed trait MorphirExpr[+A] extends Product with Serializable

sealed trait NumericExpr[+A] extends MorphirExpr[A] {
  type Number
}

object modules {
  object sdk {
    object Basics {
      // import org.finos.morphir.universe.sdk.types.Basics.Integer

      final case class Add[A](left: A, right: A) extends NumericExpr[A] {
        type Number = A
      }

      final case class Subtract[A](left: A, right: A) extends NumericExpr[A] {
        type Number = A
      }
    }
  }
}
