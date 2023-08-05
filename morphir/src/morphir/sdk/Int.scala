package morphir.sdk
import org.finos.morphir.*
import org.finos.morphir.extensibility.*

object Int extends SdkModule("Morphir.Sdk", "Int") {
  type Int8  = MInt8
  type Int16 = MInt16
  type Int32 = MInt32
  type Int64 = MInt64

  def fromInt8(value: Int8): Basics.Int   = ???
  def fromInt16(value: Int16): Basics.Int = ???
  def fromInt32(value: Int32): Basics.Int = ???
  def fromInt64(value: Int64): Basics.Int = ???
}
