package org.finos.morphir.runtime.services.sdk
import org.finos.morphir.runtime.*
import org.finos.morphir.universe.sdk.*
import org.finos.morphir.universe.sdk.types.*

trait IntModule {
  def fromInt8(value: Int8): URTAction[Basics.Integer]
  def fromInt16(value: Int16): URTAction[Basics.Integer]
  def fromInt32(value: Int32): URTAction[Basics.Integer]
  def fromInt64(value: Int64): URTAction[Basics.Integer]
}

object IntModule {
  val live: IntModule = IntModuleLive()
}

case class IntModuleLive() extends IntModule {
  def fromInt8(value: Int8): URTAction[Basics.Integer]   = URTAction.succeed(Int.fromInt8(value))
  def fromInt16(value: Int16): URTAction[Basics.Integer] = URTAction.succeed(Int.fromInt16(value))
  def fromInt32(value: Int32): URTAction[Basics.Integer] = URTAction.succeed(Int.fromInt32(value))
  def fromInt64(value: Int64): URTAction[Basics.Integer] = URTAction.succeed(Int.fromInt64(value))
}
