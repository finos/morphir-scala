package morphir.connector.github

import kyo.*

/** Supplies a GitHub access token to a live client. The host installs one named source. */
trait TokenProvider:
  def token: Token < (Abort[GithubError] & Async)

object TokenProvider:

  def const(token: Token): TokenProvider =
    ConstProvider(token)

  def constLayer(token: Token): Layer[TokenProvider, Any] =
    Layer(const(token))

  private final class ConstProvider(value: Token) extends TokenProvider:
    def token: Token < (Abort[GithubError] & Async) =
      value
