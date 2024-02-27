package org.finos.morphir.runtime
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.MorphirRuntimeError.*

//Possibly this tpe should be polymorphic on the contents
sealed trait TestTree[+T] {
  def resolveOnly = TestTree.resolveOnly(this)
}
type MorphirUnitTest = TestTree[RT]
type TestResult      = TestTree[SingleResult]

sealed trait SingleResult
object SingleResult{
  case class Passed()                         extends SingleResult
  case class Failed(msg: String)              extends SingleResult
}

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
  case class Todo(desc: String)                                  extends TestTree[Nothing]
  case class Skip(desc: String, count: Int)                      extends TestTree[Nothing]
  case class Error(desc: String, error: Throwable)  extends TestTree[Nothing]// not worth distinguishing between MorphirRuntimeError here
  case class Only[T](test: TestTree[T]) extends TestTree[T]

  def containsOnly[T](tree: TestTree[T]): Boolean =
    tree match {
      case Describe(_, tests) => tests.exists(containsOnly)
      case Concat(tests)      => tests.exists(containsOnly)
      case Only(_)            => true
      case _                  => false // If we skip an Only it does not count
    }
  def resolveOnly[T](tree: TestTree[T]): TestTree[T] =
    if (containsOnly(tree)) pruneToOnly(tree) else tree
  // Skips any tests that aren't nested under an Only
  def pruneToOnly[T](tree: TestTree[T]): TestTree[T] =
    tree match {
      case d @ Describe(desc, tests) =>
        if (containsOnly(d))
          Describe(desc, tests.map(pruneToOnly))
        else Skip(desc, count(d))
      case SingleTest(desc, _) => Skip(desc, 1)
      case Concat(tests)       => Concat(tests.map(pruneToOnly))
      case o: Only[_]             => o
      case other               => other // Skip, Todo and Err should be maintained, I think
    }

  def count[T](tree: TestTree[T]): Int =
    tree match {
      case Describe(_, tests) => tests.map(count).sum
      case _: SingleTest    => 1
      case Concat(tests)      => tests.map(count).sum
      case _: Todo            => 1
      case Skip(_, count)     => count
      case _: Error           => 1 // Might have been a suite or anything but we don't know
      case Only(inner)       => count(inner)
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
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Skip"), List(test)) =>
        fromRTValue(test) match {
          case d @ Describe(desc, tests) => Skip(desc, count(d))
          case SingleTest(desc, _)       => Skip(desc, 1)
          case other                     => Skip("", count(other))
        }
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Only"), List(test)) => Only(fromRTValue(test))
      case RT.ConstructorResult(other, _) => throw new OtherError("Unexpected constructor found", other)
      case other                          => throw new OtherError("Expected Test constructor bout found", other)
    }

}
