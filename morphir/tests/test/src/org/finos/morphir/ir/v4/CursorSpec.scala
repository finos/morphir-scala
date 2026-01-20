package org.finos.morphir.ir.v4

import zio.test._
import zio.test.Assertion._
import zio.Chunk
import org.finos.morphir.naming._

object CursorSpec extends ZIOSpecDefault {
  def spec = suite("CursorSpec")(
    suite("TypeCursor Navigation")(
      test("Should navigate down/up in Tuple") {
        val t1    = Type.Variable(TypeAttributes.empty, Name.fromString("a"))
        val t2    = Type.Variable(TypeAttributes.empty, Name.fromString("b"))
        val tuple = Type.Tuple(TypeAttributes.empty, Chunk(t1, t2))

        val cursor = TypeCursor(tuple, Nil)

        // Down to first element
        val down1 = cursor.down
        assert(down1)(isSome(hasField("current", _.current, equalTo(t1)))) &&
        // Right to second element
        assert(down1.flatMap(_.right))(isSome(hasField("current", _.current, equalTo(t2)))) &&
        // Up from second element
        assert(down1.flatMap(_.right).flatMap(_.up))(isSome(hasField("current", _.current, equalTo(tuple))))
      },
      test("Should navigate down/up in Function") {
        val arg  = Type.Variable(TypeAttributes.empty, Name.fromString("arg"))
        val ret  = Type.Variable(TypeAttributes.empty, Name.fromString("ret"))
        val func = Type.Function(TypeAttributes.empty, arg, ret)

        val cursor = TypeCursor(func, Nil)

        // Down to argument
        val downArg = cursor.down
        assert(downArg)(isSome(hasField("current", _.current, equalTo(arg)))) &&
        // Right to return type
        assert(downArg.flatMap(_.right))(isSome(hasField("current", _.current, equalTo(ret)))) &&
        // Left back to argument
        assert(downArg.flatMap(_.right).flatMap(_.left))(isSome(hasField("current", _.current, equalTo(arg)))) &&
        // Up from return type
        assert(downArg.flatMap(_.right).flatMap(_.up))(isSome(hasField("current", _.current, equalTo(func))))
      }
    ),
    suite("ValueCursor Navigation")(
      test("Should navigate down/up in Tuple") {
        val v1    = Value.Literal(ValueAttributes.empty, Literal.BoolLiteral(true))
        val v2    = Value.Literal(ValueAttributes.empty, Literal.BoolLiteral(false))
        val tuple = Value.Tuple(ValueAttributes.empty, Chunk(v1, v2))

        val cursor = ValueCursor(tuple, Nil)

        // Down to first element
        val down1 = cursor.down
        assert(down1)(isSome(hasField("current", _.current, equalTo(v1)))) &&
        // Right to second element
        assert(down1.flatMap(_.right))(isSome(hasField("current", _.current, equalTo(v2)))) &&
        // Up from second element
        assert(down1.flatMap(_.right).flatMap(_.up))(isSome(hasField("current", _.current, equalTo(tuple))))
      },
      test("Should navigate in IfThenElse") {
        val cond   = Value.Literal(ValueAttributes.empty, Literal.BoolLiteral(true))
        val thenB  = Value.Variable(ValueAttributes.empty, Name.fromString("a"))
        val elseB  = Value.Variable(ValueAttributes.empty, Name.fromString("b"))
        val ifExpr = Value.IfThenElse(ValueAttributes.empty, cond, thenB, elseB)

        val cursor = ValueCursor(ifExpr, Nil)

        // Down to condition
        val downCond = cursor.down
        assert(downCond)(isSome(hasField("current", _.current, equalTo(cond)))) &&
        // Right to thenBranch
        assert(downCond.flatMap(_.right))(isSome(hasField("current", _.current, equalTo(thenB)))) &&
        // Right to elseBranch
        assert(downCond.flatMap(_.right).flatMap(_.right))(isSome(hasField("current", _.current, equalTo(elseB)))) &&
        // Left back to thenBranch
        assert(downCond.flatMap(_.right).flatMap(_.right).flatMap(_.left))(isSome(hasField(
          "current",
          _.current,
          equalTo(thenB)
        ))) &&
        // Up from elseBranch
        assert(downCond.flatMap(_.right).flatMap(_.right).flatMap(_.up))(isSome(hasField(
          "current",
          _.current,
          equalTo(ifExpr)
        )))
      }
    )
  )
}
