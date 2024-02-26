package org.finos.morphir.runtime
import org.finos.morphir.runtime.RTValue as RT

//Possibly this tpe should be polymorphic on the contents
sealed trait TestTree[T]
type MorphirUnitTest = TestTree[RT]

sealed trait SingleResult
case class Passed extends SingleResult

object TestTree {
  case class Describe[T](desc: String, tests: List[TestTree[T]]) extends TestTree[T]
  case class SingleTest[T](desc: String, expectThunk: T)
  case class Concat[T](tests: List[TestTree[T]]) extends TestTree[T]
  case class Todo[T](desc: String)               extends TestTree[T]
  case class Skip[T](test: TestTree[T])          extends TestTree[T]
  case class Only[T](test: TestTree[T])          extends TestTree[T]

  def containsOnly[T](tree : TestTree[T]) : Boolean = {
    tree match {
      case Describe(_, tests) => tests.any(containsOnly)
      case concat(tests) => tests.any(containsOnly)
      case Skip(inner) => containsOnly(inner)
      case Only(_) => true
      case _ => false
    }
  }

  def fromRTValue(value: RT): MorphirUnitTest =
    value match {
      case RT.ConstructorResult(FQString("Morphir.UnitTest:Expect:Describe"), List(RT.String(desc), RT.List(tests))) =>
        Describe(desc, tests.map(fromRTValue))
      case RT.ConstructorResult(FQString("Morphir.UnitTest:Expect:SingleTest"), List(RT.String(desc), expectThunk)) =>
        SingleTest(desc, expectThunk)
      case RT.ConstructorResult(FQString("Morphir.UnitTest:Expect:Concat"), List(RT.List(tests))) =>
        Concat(tests.map(fromRTValue))
      case RT.ConstructorResult(FQString("Morphir.UnitTest:Expect:Todo"), List(RT.String(desc))) => Todo(desc)
      case RT.ConstructorResult(FQString("Morphir.UnitTest:Expect:Skip"), List(test)) => Only(fromRTValue(test))
      case RT.ConstructorResult(FQString("Morphir.UnitTest:Expect:Only"), List(test)) => Only(fromRTValue(test))
    }
}
