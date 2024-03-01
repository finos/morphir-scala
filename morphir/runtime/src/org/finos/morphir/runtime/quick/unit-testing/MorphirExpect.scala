package org.finos.morphir.runtime.quick
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.Pattern
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.runtime.Coercer
import org.finos.morphir.runtime.SingleTestResult
import org.finos.morphir.runtime.ErrorUtils.indentBlock
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.Extractors.Values.ApplyChain
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

//Case objects for each?
//For each of these we need:
//SDK Function which implements basic runtime behavior
//Partial function which rewrites
//Partial function which introspects + runs
//I don't think we need to enumerate case classes
sealed trait MorphirExpect {
  def arity: Int
  def funcName: String
  def fqn = FQName.fromString(UnitTesting.expectPrefix + funcName)
  def sdkFunction: SDKValue // would be nice to be able to generalize the wrapping but that's hard
  def thunkify: PartialFunction[TypedValue, TypedValue]                     = PartialFunction.empty
  def readThunk(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] = PartialFunction.empty
}

object MorphirExpect {
  case class TransparentArg(ir: TypedValue, value: RT) {
    def valueString: String = PrintRTValue(value).plainText
  }
  trait IntrospectableExpect extends MorphirExpect {
    override def thunkify: PartialFunction[TypedValue, TypedValue] = {
      case (app @ ApplyChain(Reference(_, foundFQN), args)) if (foundFQN == fqn && args.length == arity) =>
        V.lambda(
          T.function(T.unit, UnitTesting.expectationType),
          Pattern.UnitPattern(T.unit),
          app
        )
    }
    override def readThunk(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] = {
      case RT.LambdaFunction(
            ApplyChain(Reference(_, foundFQN), args),
            Pattern.UnitPattern(_),
            context
          ) if (foundFQN == fqn && args.length == arity) =>
        try
          processThunk(
            globals,
            context,
            args.map {
              arg => MorphirExpect.TransparentArg(arg, Loop(globals).loop(arg, Store(context)))
            }
          )
        catch {
          case e => SingleTestResult.Err(e)
        }
    }
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        args: List[MorphirExpect.TransparentArg]
    ): SingleTestResult
  }
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
  trait BinOpExpect extends Introspectable2 {
    def opString: String;
    def opPasses(rt1: RT, rt2: RT): Boolean

    def dynamicFunction = DynamicNativeFunction2("binOp") {
      (_: NativeContext) => (rt1: RT, rt2: RT) =>
        if (opPasses(rt1, rt2)) passedRT
        else
          failedRT(s"Expect.$funcName (${PrintRTValue(rt1).plainText}) (${PrintRTValue(rt2).plainText})")
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
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

  def expectation(result: RT) =
    RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))
  val passedRT =
    val result = RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
    expectation(result)
  def failedRT(msg: String) =
    val result =
      RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Fail"), List(RT.Primitive.String(msg)))
    expectation(result)

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
  case object Okay extends Introspectable1 {
    def funcName = "okay"

    def dynamicFunction = DynamicNativeFunction1("okay") {
      (_: NativeContext) => (value: RT.ConstructorResult) =>
        value match {
          case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Ok"), List(_)) =>
            passedRT
          case RT.ConstructorResult(FQStringTitleCase("Morphir.SDK:Result:Err"), List(err)) =>
            failedRT(s"Expect.okay recieved Err ${PrintRTValie(err).plainText}")
          case RT.ConstructorResult(_, _) =>
            throw new UnexpectedType(s"Ok(value) or Err(err)", arg, hint = "Expected due to use in a Expect.okay")
        }
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun1(dynamicFunction).realize
  }
  // This is not introspectable because the useful information largely comes from the listed functions, which are themselves introspectable
  case object All extends MorphirExpect {
    def funcName = "all"
    def arity    = 2
    // This is a bit messy: The component lambdas may already have been rewritten to produce thunks
    // Thus despite being itself a "normal" function, it may have to deal with arguments that are delayed
    def dynamicFunction = DynamicNativeFunction2("all") {
      (context: NativeContext) => (functions: RT.List, subject: RT) =>
        val withResults = functions.elements.map { f =>
          val function = f.asInstanceOf[RT.Function]
          (
            function, {
              val result = context.evaluator.handleApplyResult(T.unit, function, subject)
              // This isn't great but I don't know a better way:
              val globals = context.evaluator.asInstanceOf[Loop].globals
              evaluatedExpectToResult(globals, result)
            }
          )
        }
        val subjectString = PrintRTValue(subject).plainText
        // Get everything that failed:
        val failures = withResults.filter { case (_, result) =>
          result match {
            case SingleTestResult.Passed => false
            case _                       => true
          }
        }
        val failureStrings = withResults.collect {
          case (f, SingleTestResult.Failed(msg)) => s"${PrintRTValue(f).plainText} $subjectString failed: $msg"
          case (f, SingleTestResult.Err(err))    => s"${PrintRTValue(f).plainText} $subjectString threw error: $err"
        }

        if (failureStrings.length == 0) passedRT
        else
          failedRT(
            s"Expect.all <functions> $subjectString failed for:\n ${indentBlock(failureStrings.mkString("\n"))}"
          )
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize

  }

  case object Assert extends Introspectable1 {
    def funcName: String = "assert"
    def dynamicFunction = DynamicNativeFunction1("assert") {
      (context: NativeContext) => (result: RT.Primitive.Boolean) =>
        if (result.value) passedRT else failedRT("Assert evaluated to false")
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun1(dynamicFunction).realize
    def processThunk(
        globals: GlobalDefs,
        context: CallStackFrame,
        arg1: TransparentArg
    ): SingleTestResult =
      arg1.value match {
        case RT.Primitive.Boolean(true)  => SingleTestResult.Passed
        case RT.Primitive.Boolean(false) => SingleTestResult.Failed(explainFailure(globals, context, arg1.ir))
        case other => SingleTestResult.Err(OtherError("Assert argument did not evaluate to bool:", other))
      }
    def explainFailure(
        globals: GlobalDefs,
        context: CallStackFrame,
        ir: TypedValue
    ): String =
      ir match {
        case ApplyChain(Reference(_, FQString("Morphir.SDK:Basics:equal")), List(ir1, ir2)) =>
          val rt1        = Loop(globals).loop(ir1, Store(context))
          val rt2        = Loop(globals).loop(ir2, Store(context))
          val arg1String = ir1.toString
          val arg2String = ir2.toString
          val maxLength  = arg1String.length.max(arg2String.length)
          s"""
          assert ($arg1String) == ($arg2String) evaluated to false
              ${arg1String.padTo(maxLength, ' ')} evaluated to ${PrintRTValue(rt1).plainText}}
              ${arg2String.padTo(maxLength, ' ')} evaluated to ${PrintRTValue(rt2).plainText} """
        case ApplyChain(Reference(_, FQString(funcName)), List(ir1, ir2)) =>
          val rt1        = Loop(globals).loop(ir1, Store(context))
          val rt2        = Loop(globals).loop(ir2, Store(context))
          val arg1String = ir1.toString
          val arg2String = ir2.toString
          val maxLength  = arg1String.length.max(arg2String.length)
          s"""
          assert $funcName <arg1> <arg2> evaluated to false:
              arg1: ${arg1String.padTo(maxLength, ' ')} evaluated to ${PrintRTValue(rt1).plainText}}
              arg2: ${arg2String.padTo(maxLength, ' ')} evaluated to ${PrintRTValue(rt2).plainText} """
        case _ => s"assert $ir evaluated to false"
      }

  }

  def allExpects: List[MorphirExpect] = List(
    Equal,
    NotEqual,
    All,
    Assert,
    GreaterThan,
    LessThan,
    AtMost,
    AtLeast
  )
  def thunkifyAll: PartialFunction[TypedValue, TypedValue] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (expect.thunkify))
  def readThunkAll(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (expect.readThunk(globals)))
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
              SingleTestResult.Err(new OtherError("Unrecognized ExpectationResult: ", other))
            )
        }
      case other => readThunkAll(globals).lift(other).getOrElse(
          SingleTestResult.Err(new OtherError("Unrecognized Expectation: ", other))
        )
    }
  def newDefs: GlobalDefs =
    GlobalDefs(
      allExpects.map(expect => (expect.fqn -> expect.sdkFunction)).toMap,
      Map()
    )
}
