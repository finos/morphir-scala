package morphir.ui.github

import kyo.*
import kyo.UI
import kyo.test.*
import morphir.ui.services.*

class GitHubConnectionViewTests extends Test[Any]:

  private val sentinel = "github_pat_view_test_sentinel"

  private final class FakeSubmission(
      var token: String,
      var remember: Boolean,
      events: scala.collection.mutable.ListBuffer[String],
      name: String
  ) extends GitHubConnectionSubmission:

    def tokenValue: String < Sync = Sync.defer {
      events += s"read-token:$name"
      token
    }

    def rememberChecked: Boolean < Sync = Sync.defer {
      events += s"read-remember:$name"
      remember
    }

    def clearToken: Unit < Sync = Sync.defer {
      events += s"clear-token:$name"
      token = ""
    }

  private final class FakeForm(
      connect: FakeSubmission,
      replace: FakeSubmission,
      val events: scala.collection.mutable.ListBuffer[String]
  ) extends GitHubConnectionForm:

    def capture(target: GitHubConnectionForm.Target): GitHubConnectionSubmission < Sync = Sync.defer {
      events += s"capture:${target.formId}"
      target match
        case GitHubConnectionForm.Target.Connect => connect
        case GitHubConnectionForm.Target.Replace => replace
    }

  private object FakeForm:
    def apply(
        connectToken: String = "",
        connectRemember: Boolean = false,
        replaceToken: String = "",
        replaceRemember: Boolean = false
    ): (FakeForm, FakeSubmission, FakeSubmission) =
      val events  = scala.collection.mutable.ListBuffer.empty[String]
      val connect = FakeSubmission(connectToken, connectRemember, events, "connect")
      val replace = FakeSubmission(replaceToken, replaceRemember, events, "replace")
      (new FakeForm(connect, replace, events), connect, replace)

  private def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  private def renderState(state: GitHubConnectionStore.State): String < Async =
    val (fields, _, _) = FakeForm()
    Signal.initRef(state).map { signal =>
      GitHubConnectionView.viewWithForm(signal, fields, (_, _) => Kyo.unit, Kyo.unit)
    }.map(renderOnce)

  private def count(value: String, part: String): Int =
    value.sliding(part.length).count(_ == part)

  "GitHubConnectionView rendering" - {

    "disconnected renders a named password form with fixed safe attributes" in
      renderState(
        GitHubConnectionStore.State(GitHubConnectionStatus.Disconnected, busy = false, safeError = Absent)
      ).map { html =>
        assert(html.contains("Connections"))
        assert(html.contains("GitHub.com"))
        assert(html.contains("data-github-state=\"disconnected-idle\""))
        assert(!html.contains("<style") && !html.contains(" style="))
        assert(html.contains("id=\"github-connect-token\""))
        assert(html.contains("for=\"github-connect-token\""))
        assert(html.contains("GitHub personal access token"))
        assert(html.contains("type=\"password\""))
        assert(html.contains("autocomplete=\"off\""))
        assert(html.contains("spellcheck=\"false\""))
        assert(html.contains("autocapitalize=\"none\""))
        assert(html.contains("type=\"checkbox\""))
        assert(!html.contains(" checked"))
        assert(html.contains("Connect"))
      }

    "connecting keeps the submitted form mounted and selects disabled progress controls" in
      renderState(
        GitHubConnectionStore.State(GitHubConnectionStatus.Disconnected, busy = true, safeError = Absent)
      ).map { html =>
        assert(html.contains("data-github-state=\"disconnected-busy\""))
        assert(html.contains("id=\"github-connect-form\""))
        assert(html.contains("id=\"github-connect-token\""))
        assert(html.contains("id=\"github-connect-busy-form\""))
        assert(html.contains("id=\"github-connect-busy-token\""))
        assert(html.contains("Connecting..."))
        assert(html.contains("Checking GitHub credentials..."))
        assert(count(html, " disabled") >= 3)
      }

    "a session connection renders its login and session persistence without removing stable forms" in
      renderState(
        GitHubConnectionStore.State(
          GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Session),
          busy = false,
          safeError = Absent
        )
      ).map { html =>
        assert(html.contains("data-github-state=\"connected-idle\""))
        assert(html.contains("octocat"))
        assert(html.contains("session"))
        assert(html.contains("Disconnect"))
        assert(html.contains("id=\"github-connect-token\""))
        assert(html.contains("id=\"github-replace-token\""))
      }

    "a remembered connection renders device persistence" in
      renderState(
        GitHubConnectionStore.State(
          GitHubConnectionStatus.Connected("octocat", ConnectionPersistence.Device),
          busy = false,
          safeError = Absent
        )
      ).map { html =>
        assert(html.contains("octocat"))
        assert(html.contains("device"))
        assert(html.contains("Disconnect"))
      }

    "a rejected stored credential offers named replacement and removal controls" in
      renderState(
        GitHubConnectionStore.State(
          GitHubConnectionStatus.StoredCredentialRejected,
          busy = false,
          safeError = Absent
        )
      ).map { html =>
        assert(html.contains("data-github-state=\"rejected-idle\""))
        assert(html.contains("Stored credential rejected"))
        assert(html.contains("Replace connection"))
        assert(html.contains("Remove stored credential"))
        assert(html.contains("id=\"github-replace-token\""))
        assert(html.contains("for=\"github-replace-token\""))
      }

    "safe error copy renders as an alert" in
      renderState(
        GitHubConnectionStore.State(
          GitHubConnectionStatus.Disconnected,
          busy = false,
          safeError = Present("GitHub is unavailable. Try again.")
        )
      ).map { html =>
        assert(html.contains("role=\"alert\"") && html.contains("GitHub is unavailable. Try again."))
      }

    "form targets expose the fixed selectors shared by markup and the DOM adapter" in {
      val connect = GitHubConnectionForm.Target.Connect
      val replace = GitHubConnectionForm.Target.Replace

      assert(
        connect.formId == "github-connect-form" &&
          connect.tokenInputId == "github-connect-token" &&
          connect.rememberInputId == "github-connect-remember" &&
          replace.formId == "github-replace-form" &&
          replace.tokenInputId == "github-replace-token" &&
          replace.rememberInputId == "github-replace-remember"
      )
    }
  }

  "GitHubConnectionView submission" - {

    "captures the selected handle, reads its live fields, invokes the callback, then clears that handle" in {
      val (fields, connect, replace) = FakeForm(
        connectToken = sentinel,
        connectRemember = true,
        replaceToken = "replacement-stays",
        replaceRemember = false
      )
      var submission = Absent: Maybe[TokenSubmission]
      var remember   = Absent: Maybe[Boolean]

      GitHubConnectionView
        .submit(
          GitHubConnectionForm.Target.Connect,
          fields,
          (value, checked) =>
            Sync.defer {
              fields.events += "callback"
              submission = Present(value)
              remember = Present(checked)
            }
        )
        .map { _ =>
          assert(submission == Present(TokenSubmission.from(sentinel)))
          assert(remember == Present(true))
          assert(connect.token.isEmpty)
          assert(replace.token == "replacement-stays")
          assert(
            fields.events.toList == List(
              "capture:github-connect-form",
              "read-token:connect",
              "read-remember:connect",
              "callback",
              "clear-token:connect"
            )
          )
        }
    }

    "retains the submitted element value until a delayed callback completes" in
      Latch.init(1).map { callbackEntered =>
        Latch.init(1).map { releaseCallback =>
          val (fields, submitted, _) = FakeForm(connectToken = sentinel, connectRemember = false)

          for
            fiber <- Fiber.initUnscoped(
              GitHubConnectionView.submit(
                GitHubConnectionForm.Target.Connect,
                fields,
                (_, _) =>
                  Sync.defer {
                    fields.events += "callback-enter"
                    ()
                  }
                    .andThen(callbackEntered.release)
                    .andThen(releaseCallback.await)
                    .andThen(Sync.defer {
                      fields.events += "callback-exit"
                      ()
                    })
              )
            )
            _                   <- callbackEntered.await
            retained            <- Sync.defer(submitted.token)
            eventsBeforeRelease <- Sync.defer(fields.events.toList)
            _                   <- releaseCallback.release
            _                   <- fiber.get
          yield assert(
            retained == sentinel &&
              eventsBeforeRelease == List(
                "capture:github-connect-form",
                "read-token:connect",
                "read-remember:connect",
                "callback-enter"
              ) &&
              submitted.token.isEmpty &&
              fields.events.last == "clear-token:connect"
          )
        }
      }

    "clears the exact captured handle after a callback panic" in {
      val (fields, connect, replace) = FakeForm(
        connectToken = "connect-stays",
        replaceToken = sentinel,
        replaceRemember = true
      )

      Abort.run[Any](
        GitHubConnectionView.submit(
          GitHubConnectionForm.Target.Replace,
          fields,
          (_, _) =>
            Sync.defer {
              fields.events += "callback"
              Abort.panic(IllegalStateException("expected test panic"))
            }
        )
      ).map { result =>
        assert(result.isPanic)
        assert(connect.token == "connect-stays")
        assert(replace.token.isEmpty)
        assert(
          fields.events.toList == List(
            "capture:github-replace-form",
            "read-token:replace",
            "read-remember:replace",
            "callback",
            "clear-token:replace"
          )
        )
      }
    }

    "ordinary rendering never captures or clears an input handle" in {
      val (fields, connect, replace) = FakeForm(
        connectToken = sentinel,
        connectRemember = true,
        replaceToken = "replacement-stays"
      )
      val state = GitHubConnectionStore.State(
        GitHubConnectionStatus.Disconnected,
        busy = false,
        safeError = Absent
      )

      Signal.initRef(state).map { signal =>
        renderOnce(GitHubConnectionView.viewWithForm(signal, fields, (_, _) => Kyo.unit, Kyo.unit)).map { html =>
          assert(html.contains("type=\"password\""))
          assert(connect.token == sentinel)
          assert(replace.token == "replacement-stays")
          assert(fields.events.isEmpty)
        }
      }
    }
  }
end GitHubConnectionViewTests
