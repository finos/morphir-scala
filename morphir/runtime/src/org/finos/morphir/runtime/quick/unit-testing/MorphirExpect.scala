package org.finos.morphir.runtime.quick
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Value.Pattern
import org.finos.morphir.ir.Value.Value.{List as ListValue, *}
import org.finos.morphir.runtime.Extractors.{FQString, FQStringTitleCase}
import org.finos.morphir.runtime.Extractors.Values.ApplyChain
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  NativeFunctionAdapter
}
import org.finos.morphir.runtime.SDKValue.SDKValueDefinition
import org.finos.morphir.runtime.internal.CallStackFrame

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
  def dynamicFunction: DynamicNativeFunction // Trait is a pain, dunno what to tell you on that one
  def sdkFunction: SDKValueDefinition
  def thunkify: PartialFunction[TypedValue, TypedValue] = {
    case (app @ ApplyChain(Reference(_, fqn), args)) if (args.length == arity) =>
      V.lambda(
        T.function(T.unit, UnitTesting.expectationType),
        Pattern.UnitPattern(T.unit),
        app
      )
  }
  def readThunk: PartialFunction[RTValue, SingleTestResult] = {
    case RT.LambdaFunction(
          ApplyChain(Reference(_, fqn), args),
          Pattern.UnitPattern(_),
          context
        ) => processThunk(args, context)
  }
  def processThunk(args: List[TypedValue], context: CallStackFrame): SingleTestResult
}
sealed trait MorphirExpect1 {
  def arity = 1
  def dynamicFunction: DynamicNativeFunction1
  def sdkFunction = NativeFunctionAdapter.Fun1(
    dynamicFunction
  ).realize
}
sealed trait MorphirExpect2 {
  def arity = 2
  def dynamicFunction: DynamicNativeFunction2
  def sdkFunction = NativeFunctionAdapter.Fun2(
    dynamicFunction
  ).realize
}

object MorphirExpect {
  def expectation(result: RTValue) =
    RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))
  val passed =
    val result = RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
    expectation(result)
  def failed(msg: String) =
    val result =
      RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Fail"), List(Primitive.String(msg)))
    expectation(result)

  def extract(f: RTValue.Function, ctx: NativeContext): (TypedValue, TypedValue, RTValue, RTValue) = {
    val out = ctx.evaluator.handleApplyResult(T.unit, f, RTValue.Unit())
    val (ir1, ir2) = f match {
      case RT.LambdaFunction(Value.Tuple(_, elements), _, _) => (elements(0), elements(1))
      case other                                             => throw OtherError("This should not be!", other)
    }
    val (rt1, rt2) = out match {
      case RT.Tuple(List(rt1_, rt2_)) => (rt1_, rt2_)
      case other                      => throw new Exception("This should not be!")
    }
    (ir1, ir2, rt1, rt2)
  }

  case object Equals extends MorphirExpect {
    def arity = 2
    def equalBase = DynamicNativeFunction2("equal") {
      (_: NativeContext) => (a: RTValue, b: RTValue) =>
        val result = if (a == b) passed else failed(s"${PrintRTValue(a).plainText} != ${PrintRTValue(b).plainText}")
        expectation(result)
    }
  }

  def allExpects: List[MorphirExpect] = List()
  def convertToThunks: PartialFunction[TypedValue, TypedValue] =
    allExpects.foldLeft(PartialFunction.empty)((f, expect) => f orElse (f.thunkify))
  def readThunk: PartialFunction[
    RTValue,
    SingleTestResult
  ] // It's on the caller to handle cases that don't belong to any of these
  def newDefs: GlobalDefs =
    GlobalDefs(
      allExpects.map(expect => (expect.fqn -> expect.sdkFunction)),
      Map()
    )
}
