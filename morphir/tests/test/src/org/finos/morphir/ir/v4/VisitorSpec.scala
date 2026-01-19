package org.finos.morphir.ir.v4

import zio.test._
import zio.test.Assertion._
import zio.Chunk
import org.finos.morphir.naming._

object VisitorSpec extends ZIOSpecDefault {

  // A simple visitor that counts nodes or collects names
  class TestVisitor extends Visitor[Unit, Int] {
    override def visitType(cursor: TypeCursor, context: Unit): Int = cursor.current match {
      case Type.Variable(_, _)     => 1
      case Type.Tuple(_, elements) =>
        // Manually traverse children for test demonstration
        1 + elements.zipWithIndex.map { case (t, i) =>
          // Move down to i-th element.
          // Simplification: just assume we can navigate to children.
          // In real visitor, we'd use cursor navigation effectively.
          // For this test, we just want to see if visitType is called.
          0 // Children not traversed in this simple implementation
        }.sum
      case _ => 1
    }

    override def visitValue(cursor: ValueCursor, context: Unit): Int = cursor.current match {
      case Value.Literal(_, _) => 1
      case _                   => 1
    }
  }

  def spec = suite("VisitorSpec")(
    test("foldType should invoke visitType") {
      val t       = Type.Variable(TypeAttributes.empty, Name.fromString("x"))
      val cursor  = TypeCursor(t, Nil)
      val visitor = new TestVisitor
      val result  = Visitor.foldType(cursor, ())(visitor)
      assert(result)(equalTo(1))
    },
    test("foldValue should invoke visitValue") {
      val v       = Value.Literal(ValueAttributes.empty, Literal.BoolLiteral(true))
      val cursor  = ValueCursor(v, Nil)
      val visitor = new TestVisitor
      val result  = Visitor.foldValue(cursor, ())(visitor)
      assert(result)(equalTo(1))
    }
  )
}
