package org.finos.morphir.runtime.quick.testing
import org.finos.morphir.naming.*
import scala.collection.mutable.LinkedHashMap
import org.finos.morphir.util.{PrintRTValue, Compare, PrintDiff}
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.Pattern
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.{RTValue => RT}
import org.finos.morphir.runtime.quick.*
import org.finos.morphir.runtime.ErrorUtils.indentBlock
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.Extractors.Values.ApplyChain
import org.finos.morphir.runtime.CodeLocation
import org.finos.morphir.runtime.Extractors.{FQString, FQStringTitleCase}
import org.finos.morphir.runtime.internal.{
  CallStackFrame,
  NativeContext,
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  NativeFunctionAdapter
}
import org.finos.morphir.runtime.SDKValue
import org.finos.morphir.util.{Compare, PrintDiff}

/**
 * Expect covers representations of the various Expect.<something> functions, such as `equals` or `assert`.
 */
private[runtime] sealed trait Expect {
  def arity: Int
  // String representation of the local name of the function
  def funcName: String
  def fqn = FQName.fromString(UnitTesting.expectPrefix + funcName)

  /**
   * A "native" version of the function, which can be more powerful than the pure morphir version when it comes to
   * reporting errors. This should shadow the morphir function of the same name in globalDefs when generating the test
   * report.
   */
  def sdkFunction: SDKValue // would be nice to be able to generalize the wrapping but that's hard
  /**
   * A transformation to turn Expect function calls into lazy versions. This allows all other code (which may include a
   * lot of test organizing code) to be evaluated normally, But the actual Expect calls left un-evaluated so that they
   * may be introspected.
   */
  def thunkify: PartialFunction[TypedValue, TypedValue] = PartialFunction.empty

  /**
   * Once things have been thunkified, we need to be able to recognize the thunks and produce SingleTestREsults from
   * them.
   *
   * @param globals
   *   GlobalDefs, needed to evalute IR within the thunks. (Should include the sdkFunction of this and other expects, as
   *   defined above.)
   */
  def readThunk(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] = PartialFunction.empty
}

private[runtime] object Expect {

  /**
   * Simple helper to store both the IR of a value, and what it evaluated into. This is used for almost all of the
   * `readThunk` implementations.
   *
   * @param ir
   *   a TypedValue
   * @param value
   *   the RTValue that IR would evaluate to
   */
  case class TransparentArg(ir: TypedValue, value: RT) {
    def valueString: String =
      value.printed // A nice string representation of the RTValue (TypedValue's default toString is good enough to not warrant a similar def)
  }

  /**
   * A subtrait which handles those Expect functiosn we want to introspect.
   */
  trait IntrospectableExpect extends Expect {

    /**
     * This function converts calls like: `Expect.foo x y` to `\() -> Expect.foo(x, y)` This is used to make the Expect
     * functions introspectable.
     */
    override def thunkify: PartialFunction[TypedValue, TypedValue] = {
      case (app @ ApplyChain(Reference(_, foundFQN), args)) if (foundFQN == fqn && args.length == arity) =>
        V.lambda(
          T.function(T.unit, UnitTesting.expectationType),
          Pattern.UnitPattern(T.unit),
          app
        )
    }

    /**
     * This recognizes the `\() -> Expect.foo(x, y)` IR subtrees created by `thunkify`, evaluates their args, wraps them
     * in transparent args, and then calls `processThunk` to do the actual work.
     *
     * @param globals
     *   GlobadDefs needed to evaluate the individual arguments
     */
    override def readThunk(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] = {
      case RT.LambdaFunction(
            ApplyChain(Reference(_, foundFQN), args),
            Pattern.UnitPattern(_),
            context,
            loc
          ) if (foundFQN == fqn && args.length == arity) =>
        try
          processThunk(
            globals,
            context,
            args.map {
              arg => Expect.TransparentArg(arg, Loop(globals).loop(arg, Store(context), loc))
            }
          )
        catch {
          // We want to generically catch all errors, because that lets us report any errors thrown as failures relating to that test.
          // Given the remote, theoretical, almost laughable really possibility of us having made a coding mistake in the evaluator,
          // we make no assumptions here about what errors might be possible; this makes this a useful tool for debugging both the user
          // code and the evaluator itself.
          case e: Throwable => SingleTestResult.Err(e)
        }
    }

    /**
     * The actual work of the Expect function, specific to a particular function once the common IR matching and
     * argument evaluation has been done.
     *
     * @param globals
     *   GlobalDefs (still needed in case the expect itself contains further un-evaluated thunks, such as in
     *   `Expect.all`)
     * @param context
     *   CallStackFrame, needed for the same reason
     * @param args
     *   The arguments (both their IR and evaluated RTValue)
     */
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        args: List[Expect.TransparentArg]
    ): SingleTestResult
  }

  /**
   * Simple helper trait for arity-1 introspectable expects
   */
  trait Introspectable1 extends IntrospectableExpect {
    final def arity = 1;
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        args: List[TransparentArg]
    ): SingleTestResult =
      processThunk(
        globals,
        context,
        args(0)
      )
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg: TransparentArg
    ): SingleTestResult
  }

  /**
   * Simple helper trait for arity-2 introspectable expects
   */
  trait Introspectable2 extends IntrospectableExpect {
    final def arity = 2;
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        args: List[TransparentArg]
    ): SingleTestResult =
      processThunk(
        globals,
        context,
        args(0),
        args(1)
      )
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg1: TransparentArg,
        arg2: TransparentArg
    ): SingleTestResult
  }

  /**
   * Many of the Expect functions are binary operations, and this trait provides a common implementation for them.
   */
  trait BinOpExpect extends Introspectable2 {

    /**
     * The string (symbol) of the function, such as `==` or `>=`
     */
    def opString: String;

    /**
     * A simple function to check if the operation passes or fails
     */
    def opPasses(rt1: RT, rt2: RT): Boolean

    /**
     * A generic representation of a dynamic function, using the specific `opPasses` function to do the work.
     */
    def dynamicFunction = DynamicNativeFunction2("binOp") {
      (_: NativeContext) => (rt1: RT, rt2: RT) =>
        if (opPasses(rt1, rt2)) passedRT
        else
          failedRT(s"Expect.$funcName (${rt1.printed}) (${rt2.printed})")
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize

    /**
     * This handles the formatting of all of the Binary Op expect functions in a generic way, making it easier to tweak
     * the formatting
     */
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg1: TransparentArg,
        arg2: TransparentArg
    ): SingleTestResult =
      if (opPasses(arg1.value, arg2.value)) SingleTestResult.Passed
      else {
        val arg1String = s"${arg1.ir}"
        val arg2String = s"${arg2.ir}"
        val maxLength  = arg1String.length.max(arg2String.length)
        SingleTestResult.Failed(s"""
        Expect.$funcName (${arg1.ir}) (${arg2.ir})
            ${arg1String.padTo(maxLength, ' ')} evaluated to ${arg1.valueString}
            ${arg2String.padTo(maxLength, ' ')} evaluated to ${arg2.valueString} """)
      }

  }

  /**
   * A simple helper to wrap ap an ExpectationResult RTValue in an Expectation
   */
  def expectation(result: RT) =
    RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))

  /**
   * RTValue for a passed test
   */
  def passedRT = {
    val result = RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
    expectation(result)
  }

  /**
   * RTValue for a failed test, given some failure message
   *
   * @param msg
   *   human-readable message to be returned
   */
  def failedRT(msg: String) = {
    val result =
      RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Fail"), List(RT.Primitive.String(msg)))
    expectation(result)
  }

  case object Equal extends BinOpExpect {
    def funcName = "equal"
    def opString = "=="
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = rt1 == rt2
  }
  case object NotEqual extends BinOpExpect {
    def funcName = "notEqual"
    def opString = "!="
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = rt1 != rt2
  }

  case object GreaterThan extends BinOpExpect {
    def funcName = "greaterThan"
    def opString = ">"
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = RT.Comparable.compareOrThrow(
      Coercer.comparableCoercer.coerce(rt1),
      Coercer.comparableCoercer.coerce(rt2)
    ) > 0
  }

  case object LessThan extends BinOpExpect {
    def funcName = "lessThan"
    def opString = "<"
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = RT.Comparable.compareOrThrow(
      Coercer.comparableCoercer.coerce(rt1),
      Coercer.comparableCoercer.coerce(rt2)
    ) < 0
  }

  /**
   * This is "Less than or Equal To", but we use the name from elm-test for ease of translation
   */
  case object AtMost extends BinOpExpect {
    def funcName = "atMost"
    def opString = "<="
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = RT.Comparable.compareOrThrow(
      Coercer.comparableCoercer.coerce(rt1),
      Coercer.comparableCoercer.coerce(rt2)
    ) <= 0
  }

  /**
   * This is "Greater than or Equal To", but we use the name from elm-test for ease of translation
   */
  case object AtLeast extends BinOpExpect {
    def funcName = "atLeast"
    def opString = ">="
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = RT.Comparable.compareOrThrow(
      Coercer.comparableCoercer.coerce(rt1),
      Coercer.comparableCoercer.coerce(rt2)
    ) >= 0
  }

  /**
   * This checks that a given Result is Okay (i.e., not Error)
   */
  case object Okay extends Introspectable1 {
    def funcName = "okay"

    def dynamicFunction = DynamicNativeFunction1("okay") {
      (_: NativeContext) => (value: RT.ConstructorResult) =>
        value match {
          case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Ok"), List(_)) =>
            passedRT
          case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Err"), List(err)) =>
            failedRT(s"Expect.okay recieved Err ${err.printed}")
          case RT.ConstructorResult(_, _) =>
            throw new UnexpectedType(s"Ok(value) or Err(err)", value, hint = "Expected due to use in a Expect.okay")
        }
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun1(dynamicFunction).realize
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg1: TransparentArg
    ): SingleTestResult =
      arg1.value match {
        case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Ok"), List(_)) =>
          SingleTestResult.Passed
        case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Err"), List(err)) =>
          SingleTestResult.Failed(s"""Expect.okay ${arg1.ir} 
            ${arg1.ir} evaluated to Err ${err.printed}""")
        case _ =>
          SingleTestResult.Err(UnexpectedTypeWithIR(
            "Ok(value) or Err(err)",
            arg1.value,
            arg1.ir,
            hint = "Expected due to use in a Expect.okay"
          ))
      }
  }

  /**
   * This checks that a given Result is an Error (for use in testing unhappy paths)
   */
  case object Err extends Introspectable1 {
    def funcName = "err"
    def dynamicFunction = DynamicNativeFunction1("err") {
      (_: NativeContext) => (value: RT.ConstructorResult) =>
        value match {
          case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Ok"), List(okay)) =>
            failedRT(s"Expect.err recieved Ok ${okay.printed}")
          case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Err"), List(_)) =>
            passedRT
          case RT.ConstructorResult(_, _) =>
            throw new UnexpectedType(s"Ok(value) or Err(err)", value, hint = "Expected due to use in a Expect.err")
        }
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun1(dynamicFunction).realize
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg1: TransparentArg
    ): SingleTestResult =
      arg1.value match {
        case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Err"), List(_)) =>
          SingleTestResult.Passed
        case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Ok"), List(okay)) =>
          SingleTestResult.Failed(s"""Expect.err ${arg1.ir} 
            ${arg1.ir} evaluated to Ok ${okay.printed}""")
        case _ =>
          SingleTestResult.Err(UnexpectedTypeWithIR(
            "Ok(value) or Err(err)",
            arg1.value,
            arg1.ir,
            hint = "Expected due to use in a Expect.err"
          ))
      }
  }
  // Collection comparisons don't need to be introspectable - more important to have any decent specific diff reporting
  case object EqualLists extends Expect {
    def funcName = "equalLists"
    def arity    = 2;
    def dynamicFunction = DynamicNativeFunction2("equalLists") {
      (_: NativeContext) => (l1: RT.List, l2: RT.List) =>
        {
          val (elems1, elems2) = (l1.elements, l2.elements)
          if (elems1 == elems2) passedRT
          else {
            val compare = Compare(elems1, elems2)
            failedRT(explainFailure(elems1, elems2))
          }
        }
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
    def explainFailure(l1: List[RT], l2: List[RT]): String =
      if (l1.length != l2.length) s"Lengths differ (${l1.length} vs ${l2.length})"
      else {
        val mismatches        = l1.zip(l2).zipWithIndex.filter { case ((v1, v2), _) => v1 != v2 }
        val ((v1, v2), index) = mismatches(0)
        s"""Lists differ in ${mismatches.length} positions. 
          Example: at index $index, ${v1.printed} vs ${v2.printed}}"""
      }
  }
  case object EqualDicts extends Expect {
    def funcName = "equalDicts"
    def arity    = 2
    def dynamicFunction = DynamicNativeFunction2("equalDicts") {
      (_: NativeContext) => (l1: RT.Map, l2: RT.Map) =>
        val (elems1, elems2) = (l1.elements.toMap, l2.elements.toMap)
        if (elems1 == elems2) passedRT
        else failedRT(explainFailure(elems1, elems2))
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
    def explainFailure(l1: Map[RT, RT], l2: Map[RT, RT]): String = {
      val missingFrom1 = l1.keys.toSet.diff(l2.keys.toSet).toList
      val missingFrom2 = l2.keys.toSet.diff(l1.keys.toSet).toList
      val missing1String = if (missingFrom1.length == 0) ""
      else if (missingFrom1.length < 4) s"\n\t Keys missing from first: ${missingFrom1.map(_.printed).mkString(", ")}"
      else s"\n\t ${missingFrom1.length} keys missing including ${missingFrom1(0).printed}"
      val missing2String = if (missingFrom2.length == 0) ""
      else if (missingFrom2.length < 4) s"\n\t Keys missing from second: ${missingFrom2.map(_.printed).mkString(", ")}"
      else s"\n\t ${missingFrom2.length} keys missing including ${missingFrom2(0).printed}"
      val differing = l1.keys.toSet.intersect(l2.keys.toSet).collect {
        case key if (l1(key) != l2(key)) => (key, l1(key), l2(key))
      }.toList
      val differingString = if (differing.length == 0) ""
      else
        s"\n\t ${differing.length} keys differ including ${differing(0)._1.printed} (${differing(0)._2
            .printed} vs ${differing(0)._3.printed})"
      "Dicts were not equal:" + missing1String + missing2String + differingString
    }
  }
  case object EqualSets extends Expect {
    def funcName = "equalSets"
    def arity    = 2
    def dynamicFunction = DynamicNativeFunction2("equalSets") {
      (_: NativeContext) => (l1: RT.Set, l2: RT.Set) =>
        {
          val (elems1, elems2) = (l1.elements.toSet, l2.elements.toSet)
          if (elems1 == elems2) passedRT
          else failedRT(explainFailure(elems1, elems2))
        }
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
    def explainFailure(l1: Set[RT], l2: Set[RT]): String = {
      val missingFrom1 = l1.diff(l2).toList
      val missingFrom2 = l2.diff(l1).toList
      val missing1String = if (missingFrom1.length == 0) ""
      else if (missingFrom1.length < 4) s"\n\t Items missing from first: ${missingFrom1.map(_.printed).mkString(", ")}"
      else s"\n\t ${missingFrom1.length} items missing including ${missingFrom1(0).printed}"
      val missing2String = if (missingFrom2.length == 0) ""
      else if (missingFrom2.length < 4) s"\n\t Items missing from second: ${missingFrom2.map(_.printed).mkString(", ")}"
      else s"\n\t ${missingFrom2.length} items missing including ${missingFrom2(0).printed}"
      "Sets were not equal:" + missing1String + missing2String
    }
  }

  /**
   * Expect.all takes a list of functions that return Expectations, and a single subject, and runs all of the functiosn
   * against that expectation We do not make this introspectable because the passed arguments are already thunks, so we
   * don't need introspection to display the relevant code
   */
  case object All extends Expect {
    def funcName = "all"
    def arity    = 2

    /**
     * This function gives us an SDK variant of the basic morphir-elm implementation It has a more complex structure,
     * because the passed functions themselves may have been introspected and re-written Thus despite being a "Normal"
     * runtime function, this has to do introspection, and call the introspection handling of the argument functions
     */
    def dynamicFunction = DynamicNativeFunction2("all") {
      (context: NativeContext) => (functions: RT.List, subject: RT) =>
        val withResults = functions.elements.map { f =>
          try {
            val function = f.asInstanceOf[RT.Function]
            (
              function, {
                val result = context.evaluator.handleApplyResult(T.unit, function, subject)
                // This isn't great but I don't know a better way - we need globals distinct from context to do our more specific evalution,
                // But globals are not on the interface of generic evaluators
                val globals = context.evaluator.asInstanceOf[Loop].globals
                evaluatedExpectToResult(globals, result)
              }
            )
          } catch {
            case e: Throwable => (f, SingleTestResult.Err(e))
          }
        }
        val subjectString = subject.printed
        // Get everything that failed:
        // Note that by doing this we treat errors as failures; this may not be desireable
        val failureStrings = withResults.collect {
          case (f, SingleTestResult.Failed(msg)) => s"${f.printed} $subjectString failed: $msg"
          case (f, SingleTestResult.Err(err))    => s"${f.printed} $subjectString threw error: $err"
        }

        if (failureStrings.length == 0) passedRT
        else
          failedRT(
            s"Expect.all <functions> $subjectString failed for:\n ${indentBlock(failureStrings.mkString("\n"))}"
          )
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize

  }

  /**
   * Expect.onFail takes another Expect and, if it evaluates to a failure, replaces its error message with a
   * user-defined one Like `all` this is a special case, because it holds another Expect within it.
   */
  case object OnFail extends Expect {
    def funcName = "onFail"
    def arity    = 2

    /**
     * This function gives us an SDK variant of the basic morphir-elm implementation Like `All` above, it has to deal
     * with the possibility of its passed argument having been transformed to a thunk.
     */
    def dynamicFunction = DynamicNativeFunction2("onFail") {
      (context: NativeContext) => (msg: RT.Primitive.String, inner: RT) =>
        {
          val globals = context.evaluator.asInstanceOf[Loop].globals
          val result  = evaluatedExpectToResult(globals, inner)
          result match {
            case SingleTestResult.Failed(_) => failedRT(msg.value)
            case SingleTestResult.Passed    => passedRT
            case SingleTestResult.Err(err)  => throw err
          }
        }
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
  }

  /**
   * This is a special function not present in elm-test, inspired by ZIO's `assert`. It takes a single boolean argument,
   * but attempts to introspect the IR that created it for better using reporting.
   */
  case object Assert extends Introspectable1 {
    def funcName: String = "assert"

    /**
     * The simple SDK version of this is simple, because without being able to introspect the IR we can't do much better
     * than just reporting the boolean value
     */
    def dynamicFunction = DynamicNativeFunction1("assert") {
      (context: NativeContext) => (result: RT.Primitive.Boolean) =>
        if (result.value) passedRT else failedRT("Assert evaluated to false")
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun1(dynamicFunction).realize

    /**
     * This is the introspected version where all the magic happens. For now it only handles == specifically, with some
     * support for generic arity-2 calls, But over time it can be expanded with better reporting without breaking
     * contracts.
     */
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg1: TransparentArg
    ): SingleTestResult =
      arg1.value match {
        case RT.Primitive.Boolean(true)  => SingleTestResult.Passed
        case RT.Primitive.Boolean(false) => SingleTestResult.Failed(explainFailure(globals, context, arg1.ir))
        case _ =>
          SingleTestResult.Err(UnexpectedTypeWithIR("Bool type", arg1.value, arg1.ir, hint = "(in Expext.assert)"))
      }

    /**
     * This helper function inspects the IR and matches it against recognized patterns for more detailed reporting
     *
     * @param globals
     * @param context
     * @param ir
     */
    def explainFailure(
        globals: GlobalDefs,
        context: CallStackFrame,
        ir: TypedValue
    ): String =
      ir match {
        case ApplyChain(Reference(_, FQString("Morphir.SDK:Basics:equal")), List(ir1, ir2)) =>
          val rt1        = Loop(globals).loop(ir1, Store(context), CodeLocation.EntryPoint)
          val rt2        = Loop(globals).loop(ir2, Store(context), CodeLocation.EntryPoint)
          val arg1String = ir1.toString
          val arg2String = ir2.toString
          val maxLength  = arg1String.length.max(arg2String.length)
          s"""
          assert ($arg1String) == ($arg2String) evaluated to false
              ${arg1String.padTo(maxLength, ' ')} evaluated to ${rt1.printed}}
              ${arg2String.padTo(maxLength, ' ')} evaluated to ${rt2.printed} """
        case ApplyChain(Reference(_, FQString(funcName)), List(ir1, ir2)) =>
          val rt1        = Loop(globals).loop(ir1, Store(context), CodeLocation.EntryPoint)
          val rt2        = Loop(globals).loop(ir2, Store(context), CodeLocation.EntryPoint)
          val arg1String = ir1.toString
          val arg2String = ir2.toString
          val maxLength  = arg1String.length.max(arg2String.length)
          s"""
          assert $funcName <arg1> <arg2> evaluated to false:
              arg1: ${arg1String.padTo(maxLength, ' ')} evaluated to ${rt1.printed}}
              arg2: ${arg2String.padTo(maxLength, ' ')} evaluated to ${rt2.printed} """
        case _ => s"assert $ir evaluated to false"
      }
  }

  /**
   * All of the Expect defined above (one per Expect.<something> function that needs special handling)
   */
  def allExpects: List[Expect] = List(
    Equal,
    NotEqual,
    All,
    Assert,
    GreaterThan,
    LessThan,
    AtMost,
    AtLeast,
    Okay,
    Err,
    EqualLists,
    EqualDicts,
    EqualSets,
    OnFail
    // "Pass" and "Fail" require no special support
  )

  /**
   * This folds over all of the Expects' thunkifies, giving us a complete transformer to introspect all introspectable
   * IR
   */
  def thunkifyAll: PartialFunction[TypedValue, TypedValue] = {
    val emptyFunction: PartialFunction[TypedValue, TypedValue] = PartialFunction.empty
    allExpects.foldLeft(emptyFunction)((f: PartialFunction[TypedValue, TypedValue], expect: Expect) =>
      f orElse (expect.thunkify)
    )
  }

  /**
   * This folds over all of the Expects' readThunks, processing any thunks which were returned as a result of the
   * `thunkify` transformation
   *
   * @param globals
   *   GlobalDefs, needed by all of the Expect cases to evaluate their stuff
   */
  def readThunkAll(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] = {
    val emptyFunction: PartialFunction[RT, SingleTestResult] = PartialFunction.empty
    allExpects.foldLeft(emptyFunction)((f: PartialFunction[RT, SingleTestResult], expect: Expect) =>
      f orElse (expect.readThunk(globals))
    )
  }

  /**
   * This uses the partial `readThunkAll`, but backs it by also recognizing RTValues describing the ExpectationResult
   * type (which will have come from un-introspected Expect calls), or returns an error if nothing matches.
   *
   * @param globals
   *   GlobalDefs, needed by all of the Expect cases to evaluate their stuff
   * @param testResult
   *   The actual RTValue to be applied
   */
  def evaluatedExpectToResult(globals: GlobalDefs, testResult: RT): SingleTestResult =
    testResult match {
      case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Expect:Expectation"), List(rt)) =>
        rt match {
          case RT.ConstructorResult(FQStringTitleCase("Morphir.UnitTest:Expect:Pass"), List()) =>
            SingleTestResult.Passed
          case RT.ConstructorResult(
                FQStringTitleCase("Morphir.UnitTest:Expect:Fail"),
                List(RT.Primitive.String(msg))
              ) => SingleTestResult.Failed(msg)
          case other => readThunkAll(globals).lift(other).getOrElse(
              SingleTestResult.Err(new UnexpectedType(
                "ExpectationResult or introspectable function. (This is probably a bug in the unit testing framework)",
                other
              ))
            )
        }
      case other => readThunkAll(globals).lift(other).getOrElse(
          SingleTestResult.Err(new UnexpectedType(
            "Expectation or introspectable function. (This is probably a bug in the unit testing framework)",
            other
          ))
        )
    }

  /**
   * A GlobalDefs with all of the Expect functions above. This can be merged with the starting GlobalDefs to overwrite
   * the Expect functions with their more privileged versions.
   */
  def newDefs: GlobalDefs =
    GlobalDefs(
      allExpects.map(expect => (expect.fqn -> expect.sdkFunction)).toMap,
      Map()
    )
}
