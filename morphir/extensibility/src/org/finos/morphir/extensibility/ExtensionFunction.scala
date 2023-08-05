package org.finos.morphir.extensibility

sealed trait ExtensionFunction {
  def packageName: String
  def moduleName: String
  def localName: String
  def arity: Int

  def call(args: List[Any]): Any
}

sealed trait NativeFunction   extends ExtensionFunction
sealed trait ExternalFunction extends ExtensionFunction

trait NativeFunction0[R] extends NativeFunction with Function0[R] {
  final val arity = 0
  def apply(): R

  def call(args: List[Any]): Any = args match {
    case Nil => apply()
    case _   => throw new IllegalArgumentException(s"Expected 0 arguments but got ${args.size}")
  }
}

trait NativeFunction1[T, R] extends NativeFunction with (T => R) {
  final val arity = 1
  def apply(arg: T): R
  def call(args: List[Any]): Any = args match {
    case List(arg) => apply(arg.asInstanceOf[T])
    case _         => throw new IllegalArgumentException(s"Expected 1 arguments but got ${args.size}")
  }
}

trait NativeFunction2[T1, T2, R] extends NativeFunction with ((T1, T2) => R) {
  final val arity = 2
  def apply(arg1: T1, arg2: T2): R
  def call(args: List[Any]): Any = args match {
    case List(arg1, arg2) => apply(arg1.asInstanceOf[T1], arg2.asInstanceOf[T2])
    // TODO: Revisit error handling
    case _ => throw new IllegalArgumentException(s"Expected 2 arguments but got ${args.size}")
  }
}

final case class NativeFunc2[T1, T2, R](packageName: String, moduleName: String, localName: String, f: (T1, T2) => R)
    extends NativeFunction2[T1, T2, R] {
  def apply(arg1: T1, arg2: T2): R = f(arg1, arg2)
}
