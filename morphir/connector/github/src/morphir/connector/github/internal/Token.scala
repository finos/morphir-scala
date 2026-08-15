package morphir.connector.github
package internal

import kyo.*

/**
 * A GitHub access token. Blank input is rejected rather than stored. Logs show only a prefix and suffix.
 *
 * Public code uses this type as `morphir.connector.github.Token` via export. The class lives here so `object token` can
 * own the public JVM name `morphir.connector.github.token`.
 */
private[github] final class Token private (raw: String) derives CanEqual:
  private[github] def unsafeReveal: String = raw
  override def toString: String            = Token.redacted(raw)
  override def equals(other: Any): Boolean =
    other match
      case that: Token => unsafeReveal == that.unsafeReveal
      case _           => false
  override def hashCode: Int = 0

private[github] object Token:

  private val MinHidden        = 16
  private val SuffixLen        = 4
  private val DefaultPrefixLen = 4
  private val KnownPrefixes    = List("github_pat_", "gho_", "ghu_", "ghs_", "ghr_", "ghp_")

  def parse(raw: String): Maybe[Token] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Absent else Present(Token(trimmed))

  private def redacted(raw: String): String =
    val prefix = KnownPrefixes.find(raw.startsWith).getOrElse(raw.take(DefaultPrefixLen))
    val hidden = raw.length - prefix.length - SuffixLen
    if hidden < MinHidden then "Token(redacted)"
    else s"Token($prefix...${raw.substring(raw.length - SuffixLen)})"
