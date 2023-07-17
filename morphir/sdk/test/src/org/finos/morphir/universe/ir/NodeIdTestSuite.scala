package org.finos.morphir.universe.ir
import org.finos.morphir.testing.munit.*
import munit.Location

class NodeIdTestSuite extends MorphirTestSuite {
  describe("NodeId") {
    describe("When parsing using fromString") {
      checkParseNodeSuccess("Morphir.Sdk:Basics")(NodeId.ModuleId(
        Path("Morphir.Sdk"),
        Path("Basics")
      ))
    }
  }

  def checkParseNodeSuccess(input: String)(expected: => NodeId)(implicit loc: Location): Unit =
    test(s"Given input: `$input` s when fromString hould succeed") {
      val actual = NodeId.fromString(input)
      expectEquals(actual, Right(expected))
    }

  def checkParseNodeFailure(input: String)(implicit loc: Location): Unit =
    test(s"Parsing input: `$input` should succeed") {
      val actual = NodeId.fromString(input)
      expectEquals(actual, Left(NodeId.Error.InvalidNodeId(input)))
    }
}
