package org.finos.morphir.runtime.services.sdk
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.exports.*
import morphir.sdk.Basics
import morphir.sdk.Int as I
import morphir.sdk.Int.{Int8, Int16, Int32, Int64}

trait IntModule {
  def fromInt8(value: Int8): URTAction[Basics.Int]
  def fromInt16(value: Int16): URTAction[Basics.Int]
  def fromInt32(value: Int32): URTAction[Basics.Int]
  def fromInt64(value: Int64): URTAction[Basics.Int]
}

object IntModule {
  val live: IntModule = IntModuleLive()
}

case class IntModuleLive() extends IntModule {
  def fromInt8(value: Int8): URTAction[Basics.Int]   = URTAction.succeed(I.fromInt8(value))
  def fromInt16(value: Int16): URTAction[Basics.Int] = URTAction.succeed(I.fromInt16(value))
  def fromInt32(value: Int32): URTAction[Basics.Int] = URTAction.succeed(I.fromInt32(value))
  def fromInt64(value: Int64): URTAction[Basics.Int] = URTAction.succeed(I.fromInt64(value))
}
