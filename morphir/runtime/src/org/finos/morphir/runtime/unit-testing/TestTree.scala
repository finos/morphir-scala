package org.finos.morphir.runtime
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.MorphirRuntimeError.*

//Possibly this tpe should be polymorphic on the contents
sealed trait TestTree[T]
type MorphirUnitTest = TestTree[RT]
type TestResult      = TestTree[SingleReslt]

sealed trait SingleResult
case class Passed()                         extends SingleResult
case class Failed(msg: String)              extends SingleResult
case class Skipped(msg: String, count: Int) extends SingleResult
case class Todo(excuse: String)             extends SingleResult

object TestTree {
  case class Describe[T](desc: String, tests: List[TestTree[T]]) extends TestTree[T]
  case class SingleTest[T](desc: String, expectThunk: T)         extends TestTree[T]
  case class Concat[T](tests: List[TestTree[T]])                 extends TestTree[T]
  case class Todo[T](desc: String)                               extends TestTree[T]
  case class Skip[T](test: TestTree[T])                          extends TestTree[T]
  case class Only[T](test: TestTree[T])                          extends TestTree[T]

  def containsOnly[T](tree: TestTree[T]): Boolean =
    tree match {
      case Describe(_, tests) => tests.exists(containsOnly)
      case Concat(tests)      => tests.exists(containsOnly)
      case Skip(inner)        => containsOnly(inner)
      case Only(_)            => true
      case _                  => false
    }

  def fromRTValue(value: RT): MorphirUnitTest =
    value match {
      case RT.ConstructorResult(
            FQStringTitleCase("Morphir.UnitTest:Test:Describe"),
            List(RT.Primitive.String(desc), RT.List(tests))
          ) =>
        Describe(desc, tests.map(fromRTValue))
      case RT.ConstructorResult(
            FQStringTitleCase("Morphir.UnitTest:Test:SingleTest"),
            List(RT.Primitive.String(desc), expectThunk)
          ) =>
        SingleTest(desc, expectThunk)
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Concat"), List(RT.List(tests))) =>
        Concat(tests.map(fromRTValue))
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Todo"), List(RT.Primitive.String(desc))) =>
        Todo(desc)
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Skip"), List(test)) => Only(fromRTValue(test))
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Only"), List(test)) => Only(fromRTValue(test))
      case RT.ConstructorResult(other, _) => throw new OtherError("Unexpected constructor found", other)
      case other                          => throw new OtherError("Expected Test constructor bout found", other)
    }

}
