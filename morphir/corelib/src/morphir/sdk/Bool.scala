package morphir.sdk
import morphir.interop.extern
object Bool:
  opaque type Bool = Boolean

  def not(value:Bool):Bool = !value
  @extern def True: Bool = true
  @extern def False: Bool = false

  //

  /** Define conversions between Bool and Boolean */
  private[sdk] given Conversion[Bool, Boolean] = b => b
  private[sdk] given Conversion[Boolean, Bool] = b => b
