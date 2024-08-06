package org.finos.morphir.runtime.quick.testing
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.runtime.Extractors.*
import org.finos.morphir.runtime.SingleTestResult
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.quick.*
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.ErrorUtils.indentBlock

/**
 * This trait represents the tree of any defined Test type It is parameterized on the object stored, which changes over
 * the unit testing process
 */
private[runtime] sealed trait TestTree[+T] {

  /**
   * Checks if the tree contains an "Only" variant (which means that only tests under an "Only" should be run)
   */
  def containsOnly = TestTree.containsOnly(this)

  /**
   * Prunes the tree, removing any tests that are not nested under an "Only" variant This should be run if some
   * supertree contained `Only`, but no ancestor of this node did.
   */
  def pruneToOnly = TestTree.pruneToOnly(this)

  /**
   * Counts the number of tests in the tree (including Skipped)
   */
  def count = TestTree.count(this)
}

private[runtime] object TestTree {

  /**
   * Test.describe is used to create a suite of tests with a shared description
   */
  case class Describe[T](desc: String, tests: List[TestTree[T]]) extends TestTree[T]
  case class SingleTest[T](desc: String, expectThunk: T)         extends TestTree[T]
  case class Concat[T](tests: List[TestTree[T]])                 extends TestTree[T]
  case class Todo(desc: String)                                  extends TestTree[Nothing]
  case class Skip(desc: String, numSkipped: Int)                 extends TestTree[Nothing]

  /**
   * Because user code can generate the tests themselves, there is the possibility for that to throw an error itself. We
   * still want to run the rest of the tests and report as much as we can about where specifically the error came from,
   * so we include it in the tree
   */
  case class Error(desc: String, error: Throwable)
      extends TestTree[Nothing]

  /**
   * "Only" is a concept from elm-test; if any Only tests exist, then only those tests are run
   */
  case class Only[T](test: TestTree[T]) extends TestTree[T]

  /**
   * Once all the tests have been resolved and our leaves are `SingleTestResult`s, we can format these into a report
   */
  def toReport(tree: TestTree[SingleTestResult]): String =
    tree match {
      case Describe(desc, tests) =>
        desc + "\n" + indentBlock(tests.map(toReport).mkString("\n"))
      case SingleTest(desc, SingleTestResult.Passed)      => s"$desc: PASSED"
      case SingleTest(desc, SingleTestResult.Failed(msg)) => s"$desc: FAILED $msg"
      case SingleTest(desc, SingleTestResult.Err(err))    => s"$desc: ERROR $err"
      case Concat(tests)                                  => tests.map(toReport).mkString("\n")
      case Todo(excuse)                                   => s"$excuse: TODO"
      case Skip(desc, numSkipped) =>
        desc + ": SKIPPED" + (if (numSkipped == 1) "" else s"($numSkipped tests skipped)")
      case Error(desc, err) => s"$desc: ERROR: \n $err"
      case Only(inner)      => toReport(inner)
    }

  /**
   * Once all the tests have been resolved and our leaves are `SingleTestResult`s, we can get counts of how many passed,
   * failed, were skipped, etc
   */
  def getCounts(tree: TestTree[SingleTestResult]): TestResultCounts = {
    val empty = TestResultCounts.empty
    tree match {
      case Describe(_, tests)                        => tests.foldLeft(empty)((acc, next) => acc.plus(getCounts(next)))
      case SingleTest(_, SingleTestResult.Passed)    => empty.copy(passed = 1)
      case SingleTest(_, SingleTestResult.Failed(_)) => empty.copy(failed = 1)
      case SingleTest(_, SingleTestResult.Err(_))    => empty.copy(errors = 1)
      case Concat(tests)                             => tests.foldLeft(empty)((acc, next) => acc.plus(getCounts(next)))
      case Todo(_)                                   => empty.copy(todo = 1)
      case Skip(_, numSkipped)                       => empty.copy(skipped = numSkipped)
      case Error(_, _)                               => empty.copy(errors = 1)
      case Only(inner) =>
        getCounts(inner)
    }
  }

  /**
   * Runs the thunks users provided in `Test.test` calls
   *
   * This mostly ignores introspection, except when an introspected function is nested beneath a normal one (i.e,
   * `onFail` and `all`)
   */
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
          case e: Throwable => Error(desc, e)
        }
      case other => other // err, todo, skip lack anything to resolve
    }

  /**
   * Evaluates introspected functions, and wraps results into SingleTestResults also wraps the constructors returned by
   * non-introspected functions
   */
  def processExpects(globals: GlobalDefs)(tree: TestTree[RT]): TestTree[SingleTestResult] =
    tree match {
      case Describe(desc, tests) => Describe(desc, tests.map(processExpects(globals)))
      case Concat(tests)         => Concat(tests.map(processExpects(globals)))
      case Only(inner)           => Only(processExpects(globals)(inner))
      case SingleTest(
            desc,
            rt
          ) => SingleTest(desc, Expect.evaluatedExpectToResult(globals, rt))
      case other: Error => other
      case other: Skip  => other
      case other: Todo  => other
    }

  /**
   * Searches this tree and its subtrees for any Onlys, returning true if any are found
   */
  def containsOnly[T](tree: TestTree[T]): Boolean =
    tree match {
      case Describe(_, tests) => tests.exists(containsOnly)
      case Concat(tests)      => tests.exists(containsOnly)
      case Only(_)            => true
      case _                  => false // If we skip an Only it does not count
    }

  /**
   * Prunes the tree, removing any tests that are not nested under an "Only" variant This should be run if some
   * supertree contained `Only`, but no ancestor of this node did.
   */
  def pruneToOnly[T](tree: TestTree[T]): TestTree[T] =
    tree match {
      case d @ Describe(desc, tests) =>
        if (containsOnly(d))
          Describe(desc, tests.map(pruneToOnly))
        else Skip(desc, count(d))
      case SingleTest(desc, _) => Skip(desc, 1)
      case Concat(tests)       => Concat(tests.map(pruneToOnly))
      case o: Only[_]          => o
      case other               => other // Skip, Todo and Err should be maintained, I think
    }

  /**
   * Counts all tests in a tree, including any skipped
   */
  def count[T](tree: TestTree[T]): Int =
    tree match {
      case Describe(_, tests)  => tests.map(count).sum
      case _: SingleTest[_]    => 1
      case Concat(tests)       => tests.map(count).sum
      case _: Todo             => 1
      case Skip(_, numSkipped) => numSkipped
      case _: Error            => 1 // Might have been a suite or anything but we don't know
      case Only(inner)         => count(inner)
    }

  /**
   * Creates a TestTree from an RTValue (the result of evalauting a user-defined definition of type Test)
   *
   * @param value
   *   the RTValue resulting from the evaluation result
   */
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
          case d @ Describe(desc, _) => Skip(desc, count(d))
          case SingleTest(desc, _)   => Skip(desc, 1)
          case other                 => Skip("", count(other))
        }
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Test:Only"), List(test)) =>
        Only(fromRTValue(test))
      case otherConstructor @ RT.ConstructorResult(other, _) => throw new UnexpectedType(
          "Test",
          otherConstructor,
          hint = s"Found Constructor with unexpected FQName ${other.toStringTitleCase}"
        )
      case other => throw new UnexpectedType("Constructor (for Test)", other)
    }

}

/**
 * This case class represents the entire set of tests from some call to run unit testing
 */
private[runtime] case class TestSet[T](modules: List[ModuleTests[T]]) {
  def resolveOnly: TestSet[T] =
    if (modules.exists(_.containsOnly))
      TestSet(modules.map(_.pruneToOnly))
    else this
}
private[runtime] object TestSet {

  /**
   * Gives us the string representation of a test report
   */
  def toReport(testSet: TestSet[SingleTestResult]) = testSet.modules.map(ModuleTests.toReport(_)).mkString("\n")

  /**
   * Gives us the `TestSummary` (which includes the string report)
   *
   * @param coverageInfo
   * @param testSet
   */
  def toSummary(coverageInfo: CoverageInfo, testSet: TestSet[SingleTestResult]) =
    TestSummary(
      toReport(testSet),
      testSet.modules.map(module => (module.pkgName, module.modName) -> ModuleTests.getCounts(module)).toMap,
      coverageInfo
    )

  /**
   * Runs all of the user-defined thunks Note that
   */
  def getExpects(globals: GlobalDefs, testSet: TestSet[RT]) =
    TestSet(testSet.modules.map(ModuleTests.getExpects(globals)(_)))
  def processExpects(globals: GlobalDefs, testSet: TestSet[RT]) =
    TestSet(testSet.modules.map(ModuleTests.processExpects(globals)(_)))
}

//Represents all of the tests contained within a module
private[runtime] case class ModuleTests[T](pkgName: PackageName, modName: ModuleName, tests: List[TestTree[T]]) {
  def containsOnly: Boolean = (tests.exists(_.containsOnly))
  def pruneToOnly: ModuleTests[T] =
    if (containsOnly)
      ModuleTests(pkgName, modName, tests.map(_.pruneToOnly))
    else {
      val counted = tests.foldLeft(0)((acc, next) => acc + next.count)
      ModuleTests(pkgName, modName, List(TestTree.Skip("ModuleTests Skipped", counted)))
    }
}
private[runtime] object ModuleTests {
  def getCounts(module: ModuleTests[SingleTestResult]): TestResultCounts =
    module.tests.foldLeft(TestResultCounts.empty)((acc, next) => acc.plus(TestTree.getCounts(next)))
  def toReport(module: ModuleTests[SingleTestResult]): String = {
    val counts = getCounts(module)
    s"Module ${module.pkgName}:${module.modName} Tests:\n" +
      s"${module.tests.map(TestTree.toReport(_)).mkString("\n")}\n\n" +
      s"${module.pkgName}:${module.modName} Status - ${counts.result}\n" +
      s"$counts\n"
  }

  def getExpects(globals: GlobalDefs)(module: ModuleTests[RT]) =
    ModuleTests(module.pkgName, module.modName, module.tests.map(TestTree.getExpects(globals)(_)))
  def processExpects(globals: GlobalDefs)(module: ModuleTests[RT]) =
    ModuleTests(module.pkgName, module.modName, module.tests.map(TestTree.processExpects(globals)(_)))
}
