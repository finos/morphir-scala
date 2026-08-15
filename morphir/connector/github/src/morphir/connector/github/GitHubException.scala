package morphir.connector.github

import morphir.MorphirException

/** Failures a GitHub client can report. Usable as an exception at a user-facing boundary. */
enum GithubError(message: String) extends MorphirException(message):
  case Unauthorized(detail: String) extends GithubError(detail)
  case RateLimited(detail: String)  extends GithubError(detail)
  case GraphQl(detail: String)      extends GithubError(detail)
  case Transport(detail: String)    extends GithubError(detail)
