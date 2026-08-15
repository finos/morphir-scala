package morphir.connector.github

import kyo.*
import morphir.appkit.SecretException
import morphir.appkit.SecretStore
import morphir.connector.github.internal.GhAuth

/** Supplies a GitHub access token to a live client. The host installs one named source. */
trait TokenProvider:
  def token: Token < (Abort[GitHubException] & Async)

object TokenProvider:

  def const(token: Token): TokenProvider =
    ConstProvider(token)

  def constLayer(token: Token): Layer[TokenProvider, Any] =
    Layer(const(token))

  def flags: TokenProvider = FlagsProvider

  def flagsLayer: Layer[TokenProvider, Any] =
    Layer(flags)

  def gitHubActions: TokenProvider = GitHubActionsProvider

  def gitHubActionsLayer: Layer[TokenProvider, Any] =
    Layer(gitHubActions)

  def vault(service: String, account: String): TokenProvider < Env[SecretStore] =
    Env.use[SecretStore](store => VaultProvider(store, service, account))

  def vaultLayer(service: String, account: String): Layer[TokenProvider, Env[SecretStore]] =
    Layer(vault(service, account))

  def gitHubCli(
      user: Maybe[String] = Absent,
      hostname: Maybe[String] = Absent
  ): TokenProvider =
    gitHubCli(user, hostname, GhAuth.platform)

  def gitHubCliLayer(
      user: Maybe[String] = Absent,
      hostname: Maybe[String] = Absent
  ): Layer[TokenProvider, Any] =
    Layer(gitHubCli(user, hostname))

  private[github] def gitHubCli(
      user: Maybe[String],
      hostname: Maybe[String],
      auth: GhAuth
  ): TokenProvider =
    GitHubCliProvider(user, hostname, auth)

  private[github] def gitHubCliArgs(user: Maybe[String], hostname: Maybe[String]): Chunk[String] =
    Chunk("auth", "token")
      .concat(
        hostname match
          case Present(host) => Chunk("--hostname", host)
          case Absent        => Chunk.empty
      )
      .concat(
        user match
          case Present(login) => Chunk("--user", login)
          case Absent         => Chunk.empty
      )

  private[github] def parseFlag(value: String): Token < Abort[GitHubException] =
    parseNamed("GitHub token flag", value)

  private[github] def parseGitHubToken(value: String): Token < Abort[GitHubException] =
    parseNamed("GITHUB_TOKEN", value)

  private def parseNamed(name: String, value: String): Token < Abort[GitHubException] =
    Token.parse(value) match
      case Present(parsed) => parsed
      case Absent          => Abort.fail(GitHubException.Unauthorized(s"$name is blank"))

  private[github] def parseGhStdout(stdout: String): Token < Abort[GitHubException] =
    Token.parse(stdout) match
      case Present(parsed) => parsed
      case Absent          => Abort.fail(GitHubException.Unauthorized("gh auth token returned a blank token"))

  private final class ConstProvider(value: Token) extends TokenProvider:
    def token: Token < (Abort[GitHubException] & Async) =
      value

  private object FlagsProvider extends TokenProvider:
    def token: Token < (Abort[GitHubException] & Async) =
      parseFlag(morphir.connector.github.token())

  private object GitHubActionsProvider extends TokenProvider:
    def token: Token < (Abort[GitHubException] & Async) =
      parseGitHubToken(GITHUB_TOKEN())

  private final class VaultProvider(store: SecretStore, service: String, account: String) extends TokenProvider:
    def token: Token < (Abort[GitHubException] & Async) =
      Abort.run[SecretException](store.get(service, account)).map {
        case Result.Success(Present(secret)) => parseNamed(s"$service/$account", secret.unsafeReveal)
        case Result.Success(Absent)          =>
          Abort.fail(GitHubException.Unauthorized(s"no secret for $service/$account"))
        case Result.Failure(err) => Abort.fail(GitHubException.Unauthorized(err.getMessage))
        case Result.Panic(err)   => Abort.fail(GitHubException.Unauthorized(err.getMessage))
      }

  private final class GitHubCliProvider(
      user: Maybe[String],
      hostname: Maybe[String],
      auth: GhAuth
  ) extends TokenProvider:
    def token: Token < (Abort[GitHubException] & Async) =
      auth.stdout(gitHubCliArgs(user, hostname)).map(parseGhStdout)
