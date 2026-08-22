package morphir.ui.services

import kyo.*
import morphir.MorphirException

enum ConnectionPersistence derives CanEqual, Schema:
  case Session, Device

enum GitHubConnectionStatus derives CanEqual, Schema:
  case Disconnected
  case Connected(login: String, persistence: ConnectionPersistence)
  case StoredCredentialRejected

enum GitHubConnectionError(message: String) extends MorphirException(message) derives CanEqual, Schema:
  case RejectedToken            extends GitHubConnectionError("GitHub rejected this token.")
  case GitHubUnavailable        extends GitHubConnectionError("GitHub is unavailable. Try again.")
  case SecureStorageUnavailable extends GitHubConnectionError("Secure storage is unavailable on this device.")
  case SecureStorageFailure     extends GitHubConnectionError("The credential could not be stored securely.")
  case ExpiredLocalSession      extends GitHubConnectionError("This local session expired. Reload the page.")

trait GitHubConnectionService:
  def status(): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError])
  def connect(
      submission: TokenSubmission,
      remember: Boolean
  ): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError])
  def disconnect(): Unit < (Async & Abort[GitHubConnectionError])
