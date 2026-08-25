package morphir.web.renderer

import kyo.*
import kyo.UI.*
import kyo.test.*
import morphir.ui.github.GitHubConnectionStore
import morphir.ui.services.*

class MainTests extends Test[Any]:

  private val launch = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQ"

  private class FakeBrowser(initialFragment: String) extends Main.BrowserAdapter:
    private var currentFragment     = initialFragment
    private var transientCredential = Absent: Maybe[String]
    var events                      = Vector.empty[String]
    var exchangeCount               = 0

    def fragment(using Frame): String < Sync = Sync.defer {
      events = events :+ "fragment"
      currentFragment
    }

    def removeFragment(using Frame): Unit < Sync = Sync.defer {
      events = events :+ "replaceState"
      currentFragment = ""
    }

    def exchange(value: String)(using Frame): Unit < (Async & Abort[Closed]) = Async.defer {
      events = events :+ "exchange"
      exchangeCount += 1
      transientCredential = Present(value)
      transientCredential = Absent
    }

    def initializeApplication(using Frame): Unit < (Async & Scope & Abort[Closed]) = Async.defer {
      events = events :+ "initialize"
    }

    def retained: Boolean = currentFragment.nonEmpty || transientCredential.nonEmpty

  "Main browser startup" - {

    "removes the fragment before exchange, exchanges once, and retains no launch value" in {
      val browser = FakeBrowser(s"#launch=$launch")
      Scope.run(Main.start(browser)).map { _ =>
        assert(browser.events == Vector("fragment", "replaceState", "exchange", "initialize"))
        assert(browser.exchangeCount == 1)
        assert(!browser.retained)
      }
    }

    "reloads through the existing cookie without exchanging the removed fragment again" in {
      val browser = FakeBrowser(s"#launch=$launch")
      Scope.run(Main.start(browser)).map { _ =>
        Scope.run(Main.start(browser)).map { _ =>
          assert(browser.exchangeCount == 1)
          assert(browser.events ==
            Vector("fragment", "replaceState", "exchange", "initialize", "fragment", "initialize"))
          assert(!browser.retained)
        }
      }
    }

    "initializes an empty-fragment reload from the existing HttpOnly cookie" in {
      val browser = FakeBrowser("")
      Scope.run(Main.start(browser)).map { _ =>
        assert(browser.events == Vector("fragment", "initialize"))
        assert(browser.exchangeCount == 0)
        assert(!browser.retained)
      }
    }

    "rejects malformed nonempty duplicate and augmented fragments before exchange" in {
      val fragments = Seq(
        "#",
        "#launch=short",
        s"#other=$launch",
        s"#launch=$launch&launch=$launch",
        s"#launch=$launch&other=value",
        s"#launch=${launch.dropRight(1)}!"
      )
      Kyo.foreach(fragments) { fragment =>
        val browser = FakeBrowser(fragment)
        Scope.run(Abort.run[Closed](Main.start(browser))).map(result => (browser, result))
      }.map { results =>
        assert(results.forall { case (browser, result) => result.isFailure && browser.exchangeCount == 0 })
        assert(results.forall { case (browser, _) => !browser.events.contains("initialize") })
        assert(results.forall { case (browser, result) => !result.toString.contains(launch) && !browser.retained })
      }
    }

    "redacts startup failures from the launch value" in {
      val browser = new FakeBrowser(s"#launch=$launch"):
        override def exchange(value: String)(using Frame): Unit < (Async & Abort[Closed]) =
          Abort.fail(new Closed("BrowserSession", summon[Frame]))
      Scope.run(Abort.run[Closed](Main.start(browser))).map { result =>
        assert(result.isFailure)
        assert(!result.toString.contains(launch))
        assert(!browser.retained)
      }
    }

    "mounts the shared Connections view without CSP-blocked inline style elements" in {
      val service = new GitHubConnectionService:
        def status()                                                = GitHubConnectionStatus.Disconnected
        def connect(submission: TokenSubmission, remember: Boolean) = GitHubConnectionStatus.Disconnected
        def disconnect()                                            = ()

      GitHubConnectionStore.init(service).map { store =>
        UI.runRender(Main.connectionSettings(store)).take(1).run.map { rendered =>
          val html = rendered.mkString
          assert(html.contains("Connections"))
          assert(html.contains("GitHub.com"))
          assert(!html.contains("<style"))
        }
      }
    }

    "maps an HTTP 401 through a real handler and store to the expired-session UX" in {
      val fetch = new FetchJsonRpcTransport.Fetch:
        def post(request: FetchJsonRpcTransport.Request)(using Frame) =
          Result.succeed(FetchJsonRpcTransport.Response(401, "response-body-sentinel"))

      Scope.run {
        FetchJsonRpcTransport.init(fetch).map { transport =>
          JsonRpcHandler.init(transport).map { client =>
            GitHubConnectionStore.init(Main.remoteGitHub(client, transport)).map { store =>
              store.load.map { _ =>
                store.state.get.map { state =>
                  assert(state.safeError == Present(GitHubConnectionError.ExpiredLocalSession.getMessage))
                  assert(!state.toString.contains("response-body-sentinel"))
                }
              }
            }
          }
        }
      }
    }

    "maps network terminal failure through a real handler and store to GitHub unavailable" in {
      val fetch = new FetchJsonRpcTransport.Fetch:
        def post(request: FetchJsonRpcTransport.Request)(using Frame) =
          Result.fail(new RuntimeException("network-detail-sentinel"))

      Scope.run {
        FetchJsonRpcTransport.init(fetch).map { transport =>
          JsonRpcHandler.init(transport).map { client =>
            GitHubConnectionStore.init(Main.remoteGitHub(client, transport)).map { store =>
              store.load.map { _ =>
                store.state.get.map { state =>
                  assert(state.safeError == Present(GitHubConnectionError.GitHubUnavailable.getMessage))
                  assert(!state.toString.contains("network-detail-sentinel"))
                }
              }
            }
          }
        }
      }
    }
  }
end MainTests
