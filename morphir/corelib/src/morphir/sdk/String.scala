package morphir.sdk

import scala.Predef.{String => Str}
import scala.language.implicitConversions
object String:
  import morphir.sdk.Bool.Bool
  
  opaque type String = Str

  def isEmpty(value:String):Bool = value.isEmpty
