package org.finos.morphir.extensibility

trait ExtensionFunction {
  def packageName: String
  def moduleName: String
  def localName: String
  def arity: Int

  def call(args: List[Any]): Any
}

trait NativeFunction extends ExtensionFunction

trait NativeFunction2[T1, T2, R] extends NativeFunction with ((T1, T2) => R) {
  final val arity = 2
  def apply(arg1: T1, arg2: T2): R
  def call(args: List[Any]): Any = args match {
    case List(arg1, arg2) => apply(arg1.asInstanceOf[T1], arg2.asInstanceOf[T2])
    // TODO: Revisit error handling
    case _ => throw new IllegalArgumentException(s"Expected 2 arguments but got ${args.size}")
  }
}
