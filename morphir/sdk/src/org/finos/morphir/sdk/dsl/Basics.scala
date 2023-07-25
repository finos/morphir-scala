package org.finos.morphir.sdk.dsl
import org.finos.morphir.universe.*
object Basics {
  import modules.sdk.Basics.*

  type Int = sdk.types.Basics.Integer
  val Int: sdk.types.Basics.Integer.type = sdk.types.Basics.Integer

  def add(left: Int, right: Int): MorphirExpr[Int] = Add[Int](left, right)
}
