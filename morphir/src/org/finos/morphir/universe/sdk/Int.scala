package org.finos.morphir.universe.sdk
import zio.prelude.*
object Int {

  type Int8 = Int8.Type
  object Int8 extends Subtype[Byte] {
    def fromByte(value: Byte): Int8 = wrap(value)
  }

  type Int16 = Int16.Type
  object Int16 extends Subtype[Short] {
    def fromShort(value: Short): Int16 = wrap(value)
  }

  type Int32 = Int32.Type
  object Int32 extends Subtype[Int] {
    def fromInt(value: Int): Int32 = wrap(value)
  }

  type Int64 = Int64.Type
  object Int64 extends Subtype[Long] {
    def fromLong(value: Long): Int64 = wrap(value)
  }

  def fromInt8(value: Int8): Basics.Integer   = ???
  def fromInt16(value: Int16): Basics.Integer = ???
  def fromInt32(value: Int32): Basics.Integer = ???
  def fromInt64(value: Int64): Basics.Integer = ???
}
