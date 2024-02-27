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

//This object goes thru a series of transformations:
  // 1: A List[FQName, IR] of all Test-typed constructs
  // 2: List[FQName, Result[MorphirUnitTest]] w/ leaves consisting of RTValues (the thunks passed to test)
  // 3: Those same trees but with the thunks applied, so leaves may now contain errors as well
  // 4: Trees again, with Thunk RTValues matched (no errors), which can be either 
  //    a: fully-evaluated (for less supported things)
  //    b: Expect structures (which contain a context and IR)
  // 5: Tree with individual results
  // 6 : Final result - Formatted String, counts and result

object TestTree {
  case class Describe[T](desc: String, tests: List[TestTree[T]]) extends TestTree[T]
  case class SingleTest[T](desc: String, expectThunk: T)         extends TestTree[T]
  case class Concat[T](tests: List[TestTree[T]])                 extends TestTree[T]
  case class Todo[T](desc: String)                               extends TestTree[T]
  case class Skip[T](desc : String, count : Int)                          extends TestTree[T]
  case class Error[T](desc : String, error : MorphirRuntimeError)
  case class Only[T](test: TestTree[T])                          extends TestTree[T]

  def containsOnly[T](tree: TestTree[T]): Boolean =
    tree match {
      case Describe(_, tests) => tests.exists(containsOnly)
      case Concat(tests)      => tests.exists(containsOnly)
      case Only(_)            => true
      case _                  => false
    }
  
  def count[T](tree : TestTree[T]) : Int = {
    tree match {
      case Describe(_, tests) => tests.map(count).sum
      case _ : SingleResult => 1
      case Concat(tests) => tests.map(count).sum
      case _ : Todo => 1
      case Skip(_, count) => count
      case _ : Error => 1 //Might have been a suite or anything but we don't know
      case Only(innder) => count(inner)
    }
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
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Skip"), List(test)) => {
        fromRTValue(test) match {
          case Describe(desc, tests) => Skil(desc, )
        }
      }
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Only"), List(test)) => Only(fromRTValue(test))
      case RT.ConstructorResult(other, _) => throw new OtherError("Unexpected constructor found", other)
      case other                          => throw new OtherError("Expected Test constructor bout found", other)
    }

}
