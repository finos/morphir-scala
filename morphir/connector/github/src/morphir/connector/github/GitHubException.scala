package morphir.connector.github

import kyo.*
import morphir.MorphirException

/** Failures a GitHub client can report. Usable as an exception at a user-facing boundary. */
enum GitHubException(message: String) extends MorphirException(message):
  case Unauthorized(detail: String) extends GitHubException(detail)
  case RateLimited(detail: String)  extends GitHubException(detail)
  case GraphQl(detail: String)      extends GitHubException(detail)
  case Transport(detail: String)    extends GitHubException(detail)

object GitHubException:
  given Render[GitHubException] = Render.from { exception =>
    val variant = exception match
      case _: GitHubException.Unauthorized => "Unauthorized"
      case _: GitHubException.RateLimited  => "RateLimited"
      case _: GitHubException.GraphQl      => "GraphQl"
      case _: GitHubException.Transport    => "Transport"
    s"GitHubException.$variant: ${exception.getMessage}"
  }
