package morphir.sdk

object Bool:
  opaque type Bool = Boolean

  def not(value:Bool):Bool = !value
  def True: Bool = true
  def False: Bool = false

  //

  /** Define conversions between Bool and Boolean */
  private[sdk] given Conversion[Bool, Boolean] = b => b
  private[sdk] given Conversion[Boolean, Bool] = b => b
