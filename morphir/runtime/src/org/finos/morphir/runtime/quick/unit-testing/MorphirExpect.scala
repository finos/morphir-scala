package org.finos.morphir.runtime.quick
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.Pattern
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.runtime.SingleTestResult
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
  def sdkFunction: SDKValue // would be nice to be able to generalize the wraping but that's hard
  def thunkify: PartialFunction[TypedValue, TypedValue] = {
    case (app @ ApplyChain(Reference(_, foundFQN), args)) if (foundFQN == fqn && args.length == arity) =>
      V.lambda(
        T.function(T.unit, UnitTesting.expectationType),
        Pattern.UnitPattern(T.unit),
        app
      )
  }
  def readThunk(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] = {
    case RT.LambdaFunction(
          ApplyChain(Reference(_, fqn), args),
          Pattern.UnitPattern(_),
          context
        ) =>
      try
        processThunk(
          args.map {
            arg => MorphirExpect.TransparentArg(arg, Loop(globals).loop(arg, Store(context)))
          }
        )
      catch {
        case e => SingleTestResult.Err(e)
      }

  }
  def processThunk(
      args: List[MorphirExpect.TransparentArg]
  ): SingleTestResult
}

object MorphirExpect {
  case class TransparentArg(ir: TypedValue, value: RT) {
    def valueString: String = PrintRTValue(value).plainText
  }
  trait MorphirExpect1 extends MorphirExpect {
    final def arity = 1;
    def processThunk(args: List[TransparentArg]): SingleTestResult =
      processThunk(args(0))
    def processThunk(arg: TransparentArg): SingleTestResult
  }
  trait MorphirExpect2 extends MorphirExpect {
    final def arity = 2;
    def processThunk(args: List[TransparentArg]): SingleTestResult =
      processThunk(args(0), args(1))
    def processThunk(
        arg1: TransparentArg,
        arg2: TransparentArg
    ): SingleTestResult
  }
  trait BinOpExpect extends MorphirExpect2 {
    def opString: String;
    def opPasses(rt1: RT, rt2: RT): Boolean

    def dynamicFunction = DynamicNativeFunction2("equal") {
      (_: NativeContext) => (rt1: RT, rt2: RT) =>
        val result = if (opPasses(rt1, rt2)) passedRT
        else
          failedRT(s"Expect.$funcName (${PrintRTValue(rt1).plainText}) (${PrintRTValue(rt2).plainText})")
        expectation(result)
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
    def processThunk(
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

  def extract(f: RT.Function, ctx: NativeContext): (TypedValue, TypedValue, RT, RT) = {
    val out = ctx.evaluator.handleApplyResult(T.unit, f, RT.Unit())
    val (ir1, ir2) = f match {
      case RT.LambdaFunction(Tuple(_, elements), _, _) => (elements(0), elements(1))
      case other                                       => throw OtherError("This should not be!", other)
    }
    val (rt1, rt2) = out match {
      case RT.Tuple(List(rt1_, rt2_)) => (rt1_, rt2_)
      case other                      => throw new Exception("This should not be!")
    }
    (ir1, ir2, rt1, rt2)
  }

  case object Equal extends BinOpExpect {
    def funcName = "equal"
    def opString = "=="
    def opPasses(
        rt1: RT,
        rt2: RT
    ): Boolean = rt1 == rt2
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

  def allExpects: List[MorphirExpect] = List(
    Equal,
    GreaterThan
  )
  def thunkifyAll: PartialFunction[TypedValue, TypedValue] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (expect.thunkify))
  def readThunkAll(globals: GlobalDefs): PartialFunction[RT, SingleTestResult] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (expect.readThunk(globals)))
  def newDefs: GlobalDefs =
    GlobalDefs(
      allExpects.map(expect => (expect.fqn -> expect.sdkFunction)).toMap,
      Map()
    )
}
