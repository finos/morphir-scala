package morphir.sdk

import scala.language.implicitConversions
object Bool:
  opaque type Bool = Boolean

  def True: Bool = true
  def False: Bool = false

  /** Define conversions between Bool and Boolean */
  private[sdk] given Conversion[Bool, Boolean] = b => b
  private[sdk] given Conversion[Boolean, Bool] = b => b
