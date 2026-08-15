package morphir.connector.github

import morphir.MorphirException

/** Failures a GitHub client can report. Usable as an exception at a user-facing boundary. */
enum GitHubException(message: String) extends MorphirException(message):
  case Unauthorized(detail: String) extends GitHubException(detail)
  case RateLimited(detail: String)  extends GitHubException(detail)
  case GraphQl(detail: String)      extends GitHubException(detail)
  case Transport(detail: String)    extends GitHubException(detail)
