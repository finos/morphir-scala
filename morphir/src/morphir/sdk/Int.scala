package morphir.sdk
import morphir.sdk.extensibility.SdkModule

object Int extends SdkModule("Morphir.Sdk", "Int") {
  type Int8  = org.finos.morphir.universe.sdk.Int.Int8
  type Int16 = org.finos.morphir.universe.sdk.Int.Int16
  type Int32 = org.finos.morphir.universe.sdk.Int.Int32
  type Int64 = org.finos.morphir.universe.sdk.Int.Int64

  def fromInt8(value: Int8): Basics.Int   = ???
  def fromInt16(value: Int16): Basics.Int = ???
  def fromInt32(value: Int32): Basics.Int = ???
  def fromInt64(value: Int64): Basics.Int = ???
}
