package org.finos.morphir.universe.ir
import org.finos.morphir.testing.munit.*
import munit.Location
import org.finos.morphir.universe.ir.NodePathStep

class NodeIdTestSuite extends MorphirTestSuite {
  describe("NodeId") {
    checkParseNodeSuccess("Morphir.Sdk:Basics")(NodeId.ModuleId(
      Path("Morphir.Sdk"),
      Path("Basics")
    ))

    checkParseNodeFailure("Morphir.Sdk")

    checkToAndFromStringRoundtripping(NodeId.TypeId(FQName.fqn("Morphir.Sdk", "Basics", "Int"), NodePath.empty))
    checkToAndFromStringRoundtripping(NodeId.ValueId(FQName.fqn("Morphir.Sdk", "Basics", "add"), NodePath.empty))
    checkToAndFromStringRoundtripping(NodeId.ValueId(
      FQName.fqn("Morphir.Sdk", "LocalDate", "LocalDate"),
      NodePath(Name("day"))
    ))
  }

  def checkParseNodeSuccess(input: String)(expected: => NodeId)(implicit loc: Location): Unit =
    test(s"Given input: `$input` When fromString is called Then it should succeed") {
      val actual = NodeId.fromString(input)
      expectEquals(actual, Right(expected))
    }

  def checkParseNodeFailure(input: String)(implicit loc: Location): Unit =
    test(s"Given input: `$input` When fromString is called Then it should fail") {
      val actual = NodeId.fromString(input)
      expectEquals(actual, Left(NodeId.Error.InvalidNodeId(input)))
    }

  def checkToAndFromStringRoundtripping(input: NodeId)(implicit loc: Location): Unit =
    test(
      s"Given a NodeId (with this repr: $input) When it is converted to and then from a String Then we should get the original input"
    ) {
      val expected  = Right(input)
      val actualStr = NodeId.toString(input)
      val actual    = NodeId.fromString(actualStr)
      expectEquals(actual, expected)
    }
}
