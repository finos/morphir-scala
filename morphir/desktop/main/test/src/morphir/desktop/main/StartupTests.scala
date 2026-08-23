package morphir.desktop.main

import kyo.*
import kyo.test.*
import morphir.ui.services.GitHubConnectionError

class StartupTests extends Test[Any]:

  "Startup" - {

    "installs the handler before loading the renderer" in {
      var events = Chunk.empty[String]

      Startup
        .initialize(
          Sync.defer {
            events = events :+ "assembled"
            "github"
          },
          github =>
            Sync.defer {
              events = events :+ s"installed:$github"
            },
          Sync.defer {
            events = events :+ "loaded"
          }
        )
        .map { _ =>
          assert(events == Chunk("assembled", "installed:github", "loaded"))
        }
    }

    "fails closed without loading when assembly aborts" in {
      var events = Chunk.empty[String]

      Startup
        .failClosed[GitHubConnectionError, Sync](
          Startup.initialize(
            Abort.fail(GitHubConnectionError.SecureStorageFailure),
            _ =>
              Sync.defer {
                events = events :+ "installed"
              },
            Sync.defer {
              events = events :+ "loaded"
            }
          ),
          Sync.defer {
            events = events :+ "closed"
          }
        )
        .map { _ =>
          assert(events == Chunk("closed"))
        }
    }
  }
end StartupTests
