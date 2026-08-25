package morphir.ui.github

import kyo.*
import morphir.appkit.*
import morphir.connector.github.*
import morphir.ui.services.*

final class GitHubConnectionCoordinator private (
    verifier: GitHubTokenVerifier,
    vault: Maybe[SecretVault],
    state: AtomicRef[GitHubConnectionCoordinator.ConnectionState],
    transitionLock: TransitionLock
) extends GitHubConnectionService:

  val tokenProvider: TokenProvider = new TokenProvider:
    def token: Token < (Abort[GitHubException] & Async) =
      state.get.map { current =>
        current.token match
          case Present(token) => token
          case Absent         => Abort.fail(GitHubException.Unauthorized("No active GitHub connection"))
      }

  def status(): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
    state.get.map(_.status)

  def connect(
      submission: TokenSubmission,
      remember: Boolean
  ): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
    serialized {
      Token.parse(submission.reveal) match
        case Absent         => Abort.fail(GitHubConnectionError.RejectedToken)
        case Present(token) =>
          verify(token).map { login =>
            state.get.map { current =>
              persist(submission, remember, current.hasStoredCredential).map { _ =>
                val persistence =
                  if remember then ConnectionPersistence.Device
                  else ConnectionPersistence.Session
                val connected = GitHubConnectionStatus.Connected(login.value, persistence)
                state
                  .set(GitHubConnectionCoordinator.ConnectionState.Connected(token, login.value, persistence))
                  .andThen(connected)
              }
            }
          }
    }

  def disconnect(): Unit < (Async & Abort[GitHubConnectionError]) =
    serialized {
      state.get.map { current =>
        removeIfNeeded(current.hasStoredCredential).map { _ =>
          state.set(GitHubConnectionCoordinator.ConnectionState.Disconnected)
        }
      }
    }

  private[github] def initialize(): Unit < (Async & Abort[GitHubConnectionError]) =
    serialized {
      vault match
        case Absent               => Kyo.unit
        case Present(secretVault) =>
          Abort.run[GitHubConnectionError](readStored(secretVault)).map {
            case Result.Success(Absent)          => Kyo.unit
            case Result.Success(Present(secret)) =>
              Token.parse(secret.unsafeReveal) match
                case Absent         => state.set(GitHubConnectionCoordinator.ConnectionState.StoredCredentialRejected)
                case Present(token) => initializeStored(token)
            case Result.Failure(GitHubConnectionError.SecureStorageUnavailable) =>
              state.set(GitHubConnectionCoordinator.ConnectionState.StoredCredentialUnresolved)
            case Result.Failure(GitHubConnectionError.SecureStorageFailure) =>
              state.set(GitHubConnectionCoordinator.ConnectionState.StoredCredentialUnresolved)
            case Result.Failure(error) => Abort.fail(error)
            case Result.Panic(error)   => Sync.defer(throw error)
          }
    }

  private def initializeStored(token: Token): Unit < (Async & Abort[GitHubConnectionError]) =
    Abort.run[GitHubException](verifier.verify(token)).map {
      case Result.Success(login) =>
        state.set(
          GitHubConnectionCoordinator.ConnectionState.Connected(
            token,
            login.value,
            ConnectionPersistence.Device
          )
        )
      case Result.Failure(_: GitHubException.Unauthorized) =>
        state.set(GitHubConnectionCoordinator.ConnectionState.StoredCredentialRejected)
      case Result.Failure(_) | Result.Panic(_) =>
        state.set(GitHubConnectionCoordinator.ConnectionState.StoredCredentialUnresolved)
    }

  private def verify(token: Token): GitHubLogin < (Async & Abort[GitHubConnectionError]) =
    Abort.run[GitHubException](verifier.verify(token)).map {
      case Result.Success(login)                           => login
      case Result.Failure(_: GitHubException.Unauthorized) =>
        Abort.fail(GitHubConnectionError.RejectedToken)
      case Result.Failure(_) | Result.Panic(_) =>
        Abort.fail(GitHubConnectionError.GitHubUnavailable)
    }

  private def persist(
      submission: TokenSubmission,
      remember: Boolean,
      replacingStoredCredential: Boolean
  ): Unit < (Async & Abort[GitHubConnectionError]) =
    if !remember then removeIfNeeded(replacingStoredCredential)
    else
      vault match
        case Absent               => Abort.fail(GitHubConnectionError.SecureStorageUnavailable)
        case Present(secretVault) =>
          Secret.fromStored(submission.reveal) match
            case Absent          => Abort.fail(GitHubConnectionError.RejectedToken)
            case Present(secret) => writeStored(secretVault, secret)

  private def removeIfNeeded(removeStored: Boolean): Unit < (Async & Abort[GitHubConnectionError]) =
    if !removeStored then Kyo.unit
    else
      vault match
        case Absent               => Abort.fail(GitHubConnectionError.SecureStorageUnavailable)
        case Present(secretVault) =>
          mutateStored(secretVault.remove(GitHubConnectionCoordinator.Service, GitHubConnectionCoordinator.Account))

  private def readStored(secretVault: SecretVault): Maybe[Secret] < (Async & Abort[GitHubConnectionError]) =
    Abort.run[SecretException](
      secretVault.get(GitHubConnectionCoordinator.Service, GitHubConnectionCoordinator.Account)
    ).map {
      case Result.Success(secret)                          => secret
      case Result.Failure(_: SecretException.NotAvailable) =>
        Abort.fail(GitHubConnectionError.SecureStorageUnavailable)
      case Result.Failure(_) | Result.Panic(_) =>
        Abort.fail(GitHubConnectionError.SecureStorageFailure)
    }

  private def serialized[A](
      effect: => A < (Async & Abort[GitHubConnectionError])
  ): A < (Async & Abort[GitHubConnectionError]) =
    Abort.run[Closed](transitionLock.run(effect)).map {
      case Result.Success(value)               => value
      case Result.Failure(_) | Result.Panic(_) =>
        Abort.fail(GitHubConnectionError.GitHubUnavailable)
    }

  private def writeStored(secretVault: SecretVault, secret: Secret): Unit < (Async & Abort[GitHubConnectionError]) =
    mutateStored(
      secretVault.put(GitHubConnectionCoordinator.Service, GitHubConnectionCoordinator.Account, secret)
    )

  private def mutateStored(
      effect: Unit < (Abort[SecretException] & Async)
  ): Unit < (Async & Abort[GitHubConnectionError]) =
    Abort.run[SecretException](effect).map {
      case Result.Success(())                              => ()
      case Result.Failure(_: SecretException.NotAvailable) =>
        Abort.fail(GitHubConnectionError.SecureStorageUnavailable)
      case Result.Failure(_) | Result.Panic(_) =>
        Abort.fail(GitHubConnectionError.SecureStorageFailure)
    }

object GitHubConnectionCoordinator:
  private val Service = "org.finos.morphir"
  private val Account = "github.com"

  def init(
      verifier: GitHubTokenVerifier,
      vault: Maybe[SecretVault]
  ): GitHubConnectionCoordinator < (Async & Abort[GitHubConnectionError]) =
    TransitionLock.init.map { transitionLock =>
      init(verifier, vault, transitionLock)
    }

  private[github] def init(
      verifier: GitHubTokenVerifier,
      vault: Maybe[SecretVault],
      transitionLock: TransitionLock
  ): GitHubConnectionCoordinator < (Async & Abort[GitHubConnectionError]) =
    AtomicRef.init[ConnectionState](ConnectionState.Disconnected).map { state =>
      val coordinator = GitHubConnectionCoordinator(verifier, vault, state, transitionLock)
      coordinator.initialize().map(_ => coordinator)
    }

  private enum ConnectionState:
    case Disconnected
    case Connected(tokenValue: Token, login: String, persistence: ConnectionPersistence)
    case StoredCredentialRejected
    case StoredCredentialUnresolved

    def status: GitHubConnectionStatus = this match
      case Disconnected                     => GitHubConnectionStatus.Disconnected
      case Connected(_, login, persistence) => GitHubConnectionStatus.Connected(login, persistence)
      case StoredCredentialRejected         => GitHubConnectionStatus.StoredCredentialRejected
      case StoredCredentialUnresolved       => GitHubConnectionStatus.Disconnected

    def token: Maybe[Token] = this match
      case Connected(token, _, _) => Present(token)
      case _                      => Absent

    def hasStoredCredential: Boolean = this match
      case Connected(_, _, ConnectionPersistence.Device) | StoredCredentialRejected | StoredCredentialUnresolved =>
        true
      case _ => false

private[github] final class TransitionLock private (
    mutex: Meter,
    onContention: () => Unit < Sync
):
  def run[A, S](effect: => A < S): A < (S & Async & Abort[Closed]) =
    mutex.availablePermits.map { permits =>
      val observe =
        if permits == 0 then onContention()
        else Kyo.unit
      observe.andThen(mutex.run(effect))
    }

private[github] object TransitionLock:
  def init: TransitionLock < Sync =
    init(Kyo.unit)

  def init(onContention: => Unit < Sync): TransitionLock < Sync =
    Meter.initMutexUnscoped.map { mutex =>
      TransitionLock(mutex, () => onContention)
    }
