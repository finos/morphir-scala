package morphir.ui.services

import kyo.Schema

final class TokenSubmission private (raw: String) derives CanEqual:
  private[morphir] def reveal: String = raw

  override def toString: String = "TokenSubmission(<redacted>)"

  override def equals(other: Any): Boolean =
    other match
      case that: TokenSubmission => raw == that.reveal
      case _                     => false

  override def hashCode: Int = 0

object TokenSubmission:
  def from(raw: String): TokenSubmission = TokenSubmission(raw)

  given Schema[TokenSubmission] = summon[Schema[String]].transform(from)(_.reveal)
