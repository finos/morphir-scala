package morphir.sdk

import scala.language.implicitConversions
object MString:
  import morphir.sdk.Bool.Bool
  
  opaque type MString = String

  def isEmpty(value:MString):Bool = value.isEmpty
