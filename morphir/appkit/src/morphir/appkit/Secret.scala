package morphir.appkit

import kyo.*

/** A stored secret whose value is never exposed through ordinary rendering or inspection. */
final class Secret private (raw: String) derives CanEqual:
  private[morphir] def unsafeReveal: String = raw
  override def toString: String             = "Secret(redacted)"
  override def equals(other: Any): Boolean =
    other match
      case that: Secret => unsafeReveal == that.unsafeReveal
      case _            => false
  override def hashCode: Int = 0

object Secret:
  def fromStored(raw: String): Maybe[Secret] =
    if raw.isEmpty then Absent else Present(Secret(raw))
