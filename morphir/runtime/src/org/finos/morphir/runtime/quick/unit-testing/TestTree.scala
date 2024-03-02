package org.finos.morphir.runtime.quick
import org.finos.morphir.runtime.TestSummary
import org.finos.morphir.runtime.TestResultCounts
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.SingleTestResult
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.ErrorUtils.indentBlock

//This trait represents the tree of unit test suites at various stages of computation
sealed trait TestTree[+T] {
  def containsOnly = TestTree.containsOnly(this)
  def pruneToOnly  = TestTree.pruneToOnly(this)
  def count        = TestTree.count(this)
}

object TestTree {
  case class Describe[T](desc: String, tests: List[TestTree[T]]) extends TestTree[T]
  case class SingleTest[T](desc: String, expectThunk: T)         extends TestTree[T]
  case class Concat[T](tests: List[TestTree[T]])                 extends TestTree[T]
  case class Todo(desc: String)                                  extends TestTree[Nothing]
  case class Skip(desc: String, count: Int)                      extends TestTree[Nothing]
  case class Error(desc: String, error: Throwable)
      extends TestTree[Nothing]
  // "Only" is a concept from elm-test; if any Only tests exist, then only those tests are run
  case class Only[T](test: TestTree[T]) extends TestTree[T]

  def toReport(tree: TestTree[SingleTestResult]): String =
    tree match {
      case Describe(desc, tests) =>
        desc + "\n" + indentBlock(tests.map(toReport).mkString("\n"))
      case SingleTest(desc, SingleTestResult.Passed)      => s"$desc: PASSED"
      case SingleTest(desc, SingleTestResult.Failed(msg)) => s"$desc: FAILED $msg"
      case SingleTest(desc, SingleTestResult.Err(err))    => s"$desc: ERROR $err"
      case Concat(tests)                                  => tests.map(toReport).mkString("\n")
      case Todo(excuse)                                   => s"$excuse: TODO"
      case Skip(desc, count) =>
        desc + ": SKIPPED" + (if (count == 1) "" else s"($count tests skipped)")
      case Error(desc, err) => s"$desc: ERROR: \n $err"
      case Only(inner)      => toReport(inner)
    }
  def getCounts(tree: TestTree[SingleTestResult]): TestResultCounts =
    val empty = TestResultCounts.empty
    tree match {
      case Describe(_, tests)                     => tests.foldLeft(empty)((acc, next) => acc.plus(getCounts(next)))
      case SingleTest(_, SingleTestResult.Passed) => empty.copy(passed = 1)
      case SingleTest(_, SingleTestResult.Failed(msg)) => empty.copy(failed = 1)
      case SingleTest(_, SingleTestResult.Err(err))    => empty.copy(errors = 1)
      case Concat(tests)  => tests.foldLeft(empty)((acc, next) => acc.plus(getCounts(next)))
      case Todo(_)        => empty.copy(todo = 1)
      case Skip(_, count) => empty.copy(count = 1)
      case Error(_, _)    => empty.copy(error = 1)
      case Only(inner) =>
        getCounts(inner)

        def getExpects(globals: GlobalDefs)(test: TestTree[RT]): TestTree[RT] =
          test match {
            case Describe(desc, tests) => Describe(desc, tests.map(getExpects(globals)))
            case Concat(tests)         => Concat(tests.map(getExpects(globals)))
            case Only(inner)           => Only(getExpects(globals)(inner))

            case SingleTest(desc, thunk) =>
              try
                SingleTest(
                  desc,
                  Loop(globals)
                    .handleApplyResult(
                      UnitTesting.testType,
                      thunk,
                      RT.Unit()
                    )
                )
              catch {
                case e => Error(desc, e)
              }
            case other => other // err, todo, skip lack anything to resolve
          }

        def processExpects(globals: GlobalDefs)(tree: TestTree[RT]): TestTree[SingleTestResult] = {
          import SingleTestResult.*
          tree match {
            case Describe(desc, tests) => Describe(desc, tests.map(processExpects(globals)))
            case Concat(tests)         => Concat(tests.map(processExpects(globals)))
            case Only(inner)           => Only(processExpects(globals)(inner))
            case SingleTest(
                  desc,
                  rt
                ) => SingleTest(desc, MorphirExpect.evaluatedExpectToResult(globals, rt))
            case other: Error => other
            case other: Skip  => other
            case other: Todo  => other
          }
        }

        def containsOnly[T](tree: TestTree[T]): Boolean =
          tree match {
            case Describe(_, tests) => tests.exists(containsOnly)
            case Concat(tests)      => tests.exists(containsOnly)
            case Only(_)            => true
            case _                  => false // If we skip an Only it does not count
          }
        // Skips any tests that aren't nested under an Only
        def pruneToOnly[T](tree: TestTree[T]): TestTree[T] =
          tree match {
            case m @ ModuleTests(name, tests) =>
              if (containsOnly(m))
                ModuleTests(name, tests.map(pruneToOnly))
              else Skip("ModuleTests " + name, count(m))
            case d @ Describe(desc, tests) =>
              if (containsOnly(d))
                Describe(desc, tests.map(pruneToOnly))
              else Skip(desc, count(d))
            case SingleTest(desc, _) => Skip(desc, 1)
            case Concat(tests)       => Concat(tests.map(pruneToOnly))
            case o: Only[_]          => o
            case other               => other // Skip, Todo and Err should be maintained, I think
          }

        def count[T](tree: TestTree[T]): Int =
          tree match {
            case Describe(_, tests) => tests.map(count).sum
            case _: SingleTest[_]   => 1
            case Concat(tests)      => tests.map(count).sum
            case _: Todo            => 1
            case Skip(_, count)     => count
            case _: Error           => 1 // Might have been a suite or anything but we don't know
            case Only(inner)        => count(inner)
          }
        def fromRTValue(value: RT): TestTree[RT] =
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
            case RT.ConstructorResult(
                  FQStringTitleCase("Morphir.UnitTest:Test:Todo"),
                  List(RT.Primitive.String(desc))
                ) =>
              Todo(desc)
            case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Skip"), List(test)) =>
              fromRTValue(test) match {
                case d @ Describe(desc, tests) => Skip(desc, count(d))
                case SingleTest(desc, _)       => Skip(desc, 1)
                case other                     => Skip("", count(other))
              }
            case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Only"), List(test)) =>
              Only(fromRTValue(test))
            case RT.ConstructorResult(other, _) => throw new OtherError("Unexpected constructor found", other)
            case other                          => throw new OtherError("Expected Test constructor bout found", other)
          }

    }

  case class TestSet[T](modules: List[ModuleTests[T]]) {
    def resolveOnly[T]: TestSet[T] =
      if (modules.exists(_.containsOnly))
        TestSet(modules.map(_.pruneToOnly))
      else modules
    // Skips any tests that aren't nested under an Only
  }
  object TestSet {
    def toReport(testSet: TestSet[SingleTestResult]) = testSet.modules.map(ModuleTests.toReport(_).mkString("\n"))
    def toSummary = 
  }
  object ModuleTests {
    def getCounts(module: ModuleTests[SingleTestResult]): TestResultCounts =
      tests.foldLeft(empty)((acc, next) => acc.plus(getCounts(next)))
    def toReport(module: ModuleTests[SingleTestResult]): String =
      "ModuleTests " + module.name + " Tests:\n" + module.tests.map(TestTree.toReport(_)).mkString(
        "\n"
      ) + s"\n${getCounts(this)} \n"

    def getExpects(module: ModuleTests[RT]) =
      ModuleTests(module.name, module.tests.map(TestTree.getExpects(globals)(_)))
    def processExpects(module: ModuleTests[RT]) =
      ModuleTests(module.name, module.tests.map(TestTree.processExpects(globals)(_)))
    def pruneToOnly(module: ModuleTests[T]): ModuleTests[T] =
      if (module.tests.exists(_.containsOnly))
        ModuleTests(module.name, module.tests.map(_.pruneToOnly))
      else {
        count = module.tests.foldLeft(0)((acc, next) => acc + next.count)
        ModuleTests(module.name, TestTree.Skip("ModuleTests Skipped", count))
      }
  }
}
