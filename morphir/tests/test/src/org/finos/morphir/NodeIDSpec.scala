package org.finos.morphir
import org.finos.morphir.naming.*

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import Assertion.*

object NodeIDSpec extends MorphirBaseSpec {
  def spec = suite("NodeIDSpec")(
    checkParseNodeSuccess("Morphir.SDK:Basics")(NodeID.ModuleID(
      Path("Morphir.SDK"),
      Path("Basics")
    )),
    checkParseNodeFailure("Morphir.SDK"),
    checkToAndFromStringRoundtripping(NodeID.TypeID(FQName.fqn("Morphir.SDK", "Basics", "Int"), NodePath.empty)),
    checkToAndFromStringRoundtripping(NodeID.ValueID(FQName.fqn("Morphir.SDK", "Basics", "add"), NodePath.empty)),
    checkToAndFromStringRoundtripping(NodeID.ValueID(
      FQName.fqn("Morphir.SDK", "LocalDate", "LocalDate"),
      NodePath.fromString("day")
    ))
  )

  def checkParseNodeSuccess(input: String)(expected: => NodeID) =
    test(s"""Given input: "$input" When fromString is called Then it should succeed""") {
      val actual = NodeID.fromString(input)
      assertTrue(actual == Right(expected))
    }

  def checkParseNodeFailure(input: String) =
    test(s"""Given input: "$input" When fromString is called Then it should fail""") {
      val actual = NodeID.fromString(input)
      assert(actual)(isLeft(anything))
    }

  def checkToAndFromStringRoundtripping(input: NodeID) =
    test(
      s"""Given a NodeID (with this repr: $input) When it is converted to and then from a String Then we should get the original input"""
    ) {
      val expected  = Right(input)
      val actualStr = input.toString
      val actual    = NodeID.fromString(actualStr)
      assert(actual)(equalTo(expected))
    }
}
