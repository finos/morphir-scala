package morphir.ui.github

import kyo.*
import kyo.kernel.Effect
import morphir.ui.services.*

final class GitHubConnectionStore private (
    service: GitHubConnectionService,
    val state: SignalRef[GitHubConnectionStore.State]
):

  def load: Unit < Async =
    command(service.status())(identity)

  def connect(submission: TokenSubmission, remember: Boolean): Unit < Async =
    command(service.connect(submission, remember))(identity)

  def disconnect: Unit < Async =
    command(service.disconnect())(_ => GitHubConnectionStatus.Disconnected)

  private def command[A](
      operation: => A < (Async & Abort[GitHubConnectionError])
  )(statusFrom: A => GitHubConnectionStatus): Unit < Async =
    state.getAndUpdate { current =>
      if current.busy then current
      else current.copy(busy = true, safeError = Absent)
    }.map { previous =>
      if previous.busy then Kyo.unit
      else
        Scope.run {
          Scope.ensure(state.getAndUpdate(_.copy(busy = false)).unit).andThen {
            attempt(operation).map {
              case Result.Success(value) =>
                state.getAndUpdate(_.copy(status = statusFrom(value), safeError = Absent)).unit
              case Result.Failure(error) =>
                state.getAndUpdate(_.copy(safeError = Present(error.getMessage))).unit
              case Result.Panic(_) =>
                state
                  .getAndUpdate(_.copy(safeError = Present(GitHubConnectionError.GitHubUnavailable.getMessage)))
                  .unit
            }
          }
        }
    }

  private def attempt[A](
      operation: => A < (Async & Abort[GitHubConnectionError])
  ): Result[GitHubConnectionError, A] < Async =
    Effect.catching(Abort.run[GitHubConnectionError](operation)) { (error: Throwable) =>
      Result.Panic(error): Result[GitHubConnectionError, A]
    }

object GitHubConnectionStore:

  final case class State(
      status: GitHubConnectionStatus,
      busy: Boolean,
      safeError: Maybe[String]
  ) derives CanEqual

  def init(
      service: GitHubConnectionService,
      initialStatus: GitHubConnectionStatus = GitHubConnectionStatus.Disconnected
  ): GitHubConnectionStore < Sync =
    Signal
      .initRef(State(initialStatus, busy = false, safeError = Absent))
      .map(GitHubConnectionStore(service, _))
