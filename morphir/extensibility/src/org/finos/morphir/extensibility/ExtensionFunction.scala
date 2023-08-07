package org.finos.morphir.extensibility
import org.finos.morphir.*

sealed trait ExtensionFunction {
  def packageName: String
  def moduleName: String
  def localName: String
  def arity: Int
  def defaultHints: Hints = Hints.empty

  def invoke(args: List[Any], hints: Hints): Any
}

trait DynamicFunction extends ExtensionFunction {
  def invokeDynamic(args: List[Any], hints: Hints): Any
  @inline final def invoke(args: List[Any], hints: Hints): Any = invokeDynamic(args, hints)
}

trait NativeFunction   extends ExtensionFunction
trait ExternalFunction extends ExtensionFunction

// trait NativeFunction0[R] extends NativeFunction with Function0[R] {
//   type Result = R
//   final val arity = 0
//   def apply(): R

//   def invoke(args: List[Any]): Any = args match {
//     case Nil => apply()
//     case _   => throw new IllegalArgumentException(s"Expected 0 arguments but got ${args.size}")
//   }
// }

// trait NativeFunction1[T, R] extends NativeFunction with (T => R) {
//   type Result = R

//   final val arity = 1
//   def apply(arg: T): R
//   def invoke(args: List[Any]): Any = args match {
//     case List(arg) => apply(arg.asInstanceOf[T])
//     case _         => throw new IllegalArgumentException(s"Expected 1 arguments but got ${args.size}")
//   }
// }

trait NativeFunction2[-T1, -T2, +R] extends NativeFunction with ((T1, T2) => R) {
  final val arity                        = 2
  final def apply(arg1: T1, arg2: T2): R = invokeStrict(arg1, arg2)(defaultHints)

  def invokeStrict(arg1: T1, arg2: T2)(implicit hints: Hints = defaultHints): R
  def invokeDynamic(arg1: Any, arg2: Any): Any               = invokeDynamic(arg1, arg2, defaultHints)
  def invokeDynamic(arg1: Any, arg2: Any, hints: Hints): Any = apply(arg1.asInstanceOf[T1], arg2.asInstanceOf[T2])
  def invoke(args: List[Any], hints: Hints): Any = args match {
    case List(arg1, arg2) => invokeDynamic(arg1, arg2)
    // TODO: Revisit error handling
    case _ => throw new IllegalArgumentException(s"Expected 2 arguments but got ${args.size}")
  }
}

final case class NativeFunc2[T1, T2, R](packageName: String, moduleName: String, localName: String, f: (T1, T2) => R)
    extends NativeFunction2[T1, T2, R] {
  def invokeStrict(arg1: T1, arg2: T2)(implicit hints: Hints = defaultHints): R = f(arg1, arg2)
}

trait IntBinaryOperator extends NativeFunction2[MInt, MInt, MInt]

final case class IntBinaryOp[T <: MInt](
    packageName: String,
    moduleName: String,
    localName: String,
    f: (T, T) => T
) extends IntBinaryOperator {
  def invokeStrict(arg1: MInt, arg2: MInt)(implicit hints: Hints = defaultHints): MInt =
    f(arg1.asInstanceOf[T], arg2.asInstanceOf[T])
}

sealed trait SdkFunction extends NativeFunction {}
