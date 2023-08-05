package org.finos.morphir.extensibility
import scala.collection.mutable.{IndexedSeq => MutIndexedSeq}
trait MorphirModule {
  def packageName: String
  def moduleName: String

  private val nativeFunctions: MutIndexedSeq[NativeFunction] = MutIndexedSeq.empty[NativeFunction]

  private def registerNativeFunction[Func <: NativeFunction](nativeFunction: Func): Func = {
    nativeFunctions :+ nativeFunction
    nativeFunction
  }

  def fun[T1, T2, R](localName: String)(f: (T1, T2) => R): NativeFunc2[T1, T2, R] =
    registerNativeFunction(NativeFunc2(packageName, moduleName, localName, f))
}
