package morphir.connector.github

import kyo.*
import morphir.MorphirException

/** Failures a GitHub client can report. Usable as an exception at a user-facing boundary. */
enum GitHubException(message: String) extends MorphirException(message):
  case Unauthorized(detail: String) extends GitHubException(detail)
  case Forbidden(detail: String)    extends GitHubException(detail)
  case RateLimited(detail: String)  extends GitHubException(detail)
  case GraphQl(detail: String)      extends GitHubException(detail)
  case Transport(detail: String)    extends GitHubException(detail)

object GitHubException:
  given Render[GitHubException] = Render.from { exception =>
    val variant = exception match
      case _: GitHubException.Unauthorized => "Unauthorized"
      case _: GitHubException.Forbidden    => "Forbidden"
      case _: GitHubException.RateLimited  => "RateLimited"
      case _: GitHubException.GraphQl      => "GraphQl"
      case _: GitHubException.Transport    => "Transport"
    s"GitHubException.$variant: ${exception.getMessage}"
  }

  /** Map an HTTP status and response detail to a typed failure. A 403 is rate-limited only when the
    * detail looks like GitHub's primary or secondary rate-limit wording; other 403s are Forbidden.
    */
  private[github] def fromHttpStatus(code: Int, detail: String): GitHubException =
    code match
      case 401 => Unauthorized(detail)
      case 403 =>
        if looksRateLimited(detail) then RateLimited(detail) else Forbidden(detail)
      case _ => Transport(detail)

  private[github] def looksRateLimited(detail: String): Boolean =
    val lower = detail.toLowerCase
    lower.contains("rate limit") ||
      lower.contains("secondary rate") ||
      lower.contains("abuse detection")
