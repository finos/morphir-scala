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
  def dynamicFunction: DynamicNativeFunction
  def sdkFunction: SDKValue // would be nice to
  def thunkify: PartialFunction[TypedValue, TypedValue] = {
    case (app @ ApplyChain(Reference(_, fqn), args)) if (args.length == arity) =>
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
        ) => processThunk(globals, args, context)
  }
  def processThunk(globals: GlobalDefs, args: List[TypedValue], context: CallStackFrame): SingleTestResult
}

object MorphirExpect {
  def expectation(result: RT) =
    RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))
  val passed =
    val result = RT.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
    expectation(result)
  def failed(msg: String) =
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

  case object Equals extends MorphirExpect {
    def arity    = 2
    def funcName = "equal"
    def dynamicFunction = DynamicNativeFunction2("equal") {
      (_: NativeContext) => (a: RT, b: RT) =>
        val result = if (a == b) passed else failed(s"${PrintRTValue(a).plainText} != ${PrintRTValue(b).plainText}")
        expectation(result)
    }
    def sdkFunction: SDKValue = NativeFunctionAdapter.Fun2(dynamicFunction).realize
    def processThunk(globals: GlobalDefs, args: List[TypedValue], context: CallStackFrame): SingleTestResult = {
      // We need globals here
    }
  }

  def allExpects: List[MorphirExpect] = List()
  def thunkifyAll: PartialFunction[TypedValue, TypedValue] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (expect.thunkify))
  def readThunkAll: PartialFunction[RT, SingleTestResult] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (expect.readThunk))
  def newDefs: GlobalDefs =
    GlobalDefs(
      allExpects.map(expect => (expect.fqn -> expect.sdkFunction)).toMap,
      Map()
    )
}
