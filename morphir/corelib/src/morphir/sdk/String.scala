package morphir.sdk
import morphir.interop.*

import scala.Predef.{String => Str}
import scala.language.implicitConversions
object String:
  import morphir.sdk.Bool.Bool
  
  opaque type String = Str

  @extern def isEmpty(value:String):Bool = value.isEmpty
