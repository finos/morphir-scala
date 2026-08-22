package morphir.connector.github

import kyo.*
import morphir.connector.github.internal.GraphQl
import morphir.connector.github.internal.PlatformLive

/** The validated GitHub.com login returned for an authenticated token. */
opaque type GitHubLogin = String

object GitHubLogin:
  given CanEqual[GitHubLogin, GitHubLogin] = CanEqual.derived

  def parse(raw: String): Maybe[GitHubLogin] =
    if valid(raw) then Present(raw) else Absent

  extension (login: GitHubLogin) def value: String = login

  private def valid(raw: String): Boolean =
    raw.length >= 1 &&
      raw.length <= 39 &&
      asciiAlphaNumeric(raw.head) &&
      asciiAlphaNumeric(raw.last) &&
      raw.forall(char => asciiAlphaNumeric(char) || char == '-') &&
      !raw.contains("--")

  private def asciiAlphaNumeric(char: Char): Boolean =
    (char >= 'A' && char <= 'Z') ||
      (char >= 'a' && char <= 'z') ||
      (char >= '0' && char <= '9')

/** Compile-time validated GitHub logins: `gitHubLogin"octocat"`. */
extension (inline sc: StringContext)
  inline def gitHubLogin(inline args: Any*): GitHubLogin = ${ GitHubLoginMacros.gitHubLoginImpl('sc, 'args) }

private[github] object GitHubLoginMacros:
  import scala.quoted.*

  def gitHubLoginImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[GitHubLogin] =
    import quotes.reflect.report
    args match
      case Varargs(Seq()) => ()
      case _ => report.errorAndAbort("gitHubLogin accepts no interpolated arguments; use GitHubLogin.parse instead")
    val literal = sc.valueOrAbort.parts.mkString
    if !"[A-Za-z0-9](?:[A-Za-z0-9]|-(?=[A-Za-z0-9])){0,38}".r.matches(literal) then
      report.errorAndAbort(s"invalid GitHub login: $literal")
    '{ ${ Expr(literal) }.asInstanceOf[GitHubLogin] }

/** Verifies a GitHub.com token by reading the authenticated viewer login. */
trait GitHubTokenVerifier:
  def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async)

object GitHubTokenVerifier:

  private val TokenRejected = "GitHub token was rejected"
  private val RequestFailed = "GitHub request failed"

  /** A verifier that decodes a recorded GitHub GraphQL response without making an HTTP request. */
  def recorded(json: String): GitHubTokenVerifier =
    Recorded(json)

  /** Stateless GitHub.com verifier backed by the platform's authenticated GraphQL sender. */
  val live: GitHubTokenVerifier =
    Live

  private[github] def httpFailure(code: Int): GitHubException =
    code match
      case 401 | 403 => GitHubException.Unauthorized(TokenRejected)
      case _         => GitHubException.Transport(RequestFailed)

  private object Live extends GitHubTokenVerifier:
    def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async) =
      PlatformLive.verify(token)

  private final case class Recorded(json: String) extends GitHubTokenVerifier:
    def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async) =
      val _ = token
      GitHubTokenVerifier.lift(GraphQl.decodeViewerLogin(json))

  private[github] def lift[A](result: Result[GitHubException, A]): A < Abort[GitHubException] =
    result match
      case Result.Success(value) => value
      case Result.Failure(err)   => Abort.fail(err)
      case Result.Panic(err)     => Abort.fail(GitHubException.GraphQl(err.getMessage))
