package morphir.ui.github

import kyo.*
import kyo.test.*
import morphir.ui.services.*

class GitHubConnectionStoreTests extends Test[Any]:

  private val sentinel = "github_pat_store_test_sentinel"

  private final class FakeService extends GitHubConnectionService:
    var statusCalls: Int                                                                    = 0
    var connectCalls: Int                                                                   = 0
    var disconnectCalls: Int                                                                = 0
    var receivedSubmission: Maybe[TokenSubmission]                                          = Absent
    var receivedRemember: Maybe[Boolean]                                                    = Absent
    var statusResult: () => GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
      () => GitHubConnectionStatus.Disconnected
    var connectResult: () => GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
      () => GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Session)
    var disconnectResult: () => Unit < (Async & Abort[GitHubConnectionError]) = () => Kyo.unit

    def status(): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
      statusCalls += 1
      statusResult()

    def connect(
        submission: TokenSubmission,
        remember: Boolean
    ): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
      connectCalls += 1
      receivedSubmission = Present(submission)
      receivedRemember = Present(remember)
      connectResult()

    def disconnect(): Unit < (Async & Abort[GitHubConnectionError]) =
      disconnectCalls += 1
      disconnectResult()

  "GitHubConnectionStore" - {

    "load replaces the initial disconnected status with the service status" in {
      val service  = FakeService()
      val expected = GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device)
      service.statusResult = () => expected

      for
        store <- GitHubConnectionStore.init(service)
        _     <- store.load
        state <- store.state.get
      yield assert(state == GitHubConnectionStore.State(expected, busy = false, safeError = Absent))
    }

    "connect forwards the one-use submission and remember choice then stores only safe status" in {
      val service    = FakeService()
      val submission = TokenSubmission.from(sentinel)
      val expected   = GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device)
      service.connectResult = () => expected

      for
        store <- GitHubConnectionStore.init(service)
        _     <- store.connect(submission, remember = true)
        state <- store.state.get
      yield
        assert(service.receivedSubmission == Present(submission))
        assert(service.receivedRemember == Present(true))
        assert(state == GitHubConnectionStore.State(expected, busy = false, safeError = Absent))
        assert(state.productArity == 3)
        assert(!state.toString.contains(sentinel))
    }

    "disconnect clears a connected status after the service succeeds" in {
      val service = FakeService()
      val initial = GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Session)

      for
        store <- GitHubConnectionStore.init(service, initial)
        _     <- store.disconnect
        state <- store.state.get
      yield assert(
        service.disconnectCalls == 1 &&
          state == GitHubConnectionStore.State(
            GitHubConnectionStatus.Disconnected,
            busy = false,
            safeError = Absent
          )
      )
    }

    "a second command is ignored while the first command is busy" in
      Latch.init(1).map { entered =>
        Latch.init(1).map { release =>
          val service = FakeService()
          service.connectResult = () =>
            entered.release
              .andThen(release.await)
              .andThen(GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Session))

          for
            store      <- GitHubConnectionStore.init(service)
            firstFiber <- Fiber.initUnscoped(store.connect(TokenSubmission.from(sentinel), remember = false))
            _          <- entered.await
            busy       <- store.state.get
            _          <- store.disconnect
            during     <- store.state.get
            _          <- release.release
            _          <- firstFiber.get
            after      <- store.state.get
          yield assert(
            busy.busy && during.busy && service.connectCalls == 1 && service.disconnectCalls == 0 &&
              !after.busy
          )
        }
      }

    "a typed failure copies only its safe message and resets busy" in {
      val service = FakeService()
      service.connectResult = () => Abort.fail(GitHubConnectionError.RejectedToken)

      for
        store <- GitHubConnectionStore.init(service)
        _     <- store.connect(TokenSubmission.from(sentinel), remember = false)
        state <- store.state.get
      yield assert(
        state == GitHubConnectionStore.State(
          GitHubConnectionStatus.Disconnected,
          busy = false,
          safeError = Present("GitHub rejected this token.")
        ) && !state.toString.contains(sentinel)
      )
    }

    "a panic becomes generic safe copy and resets busy without retaining panic text" in {
      val service       = FakeService()
      val unsafeMessage = s"transport failed with $sentinel"
      service.statusResult = () => Abort.panic(IllegalStateException(unsafeMessage))

      for
        store <- GitHubConnectionStore.init(service)
        _     <- store.load
        state <- store.state.get
      yield assert(
        state == GitHubConnectionStore.State(
          GitHubConnectionStatus.Disconnected,
          busy = false,
          safeError = Present("GitHub is unavailable. Try again.")
        ) && !state.toString.contains(unsafeMessage)
      )
    }

    "interrupting a blocked command still resets busy" in
      Latch.init(1).map { entered =>
        Latch.init(1).map { neverReleased =>
          val service = FakeService()
          service.connectResult = () =>
            entered.release
              .andThen(neverReleased.await)
              .andThen(GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Session))

          for
            store <- GitHubConnectionStore.init(service)
            fiber <- Fiber.initUnscoped(store.connect(TokenSubmission.from(sentinel), remember = false))
            _     <- entered.await
            _     <- fiber.interrupt
            _     <- fiber.getResult
            state <- store.state.streamCurrent.filter(!_.busy).take(1).run.map(_.head)
          yield assert(!state.busy)
        }
      }
  }
end GitHubConnectionStoreTests
