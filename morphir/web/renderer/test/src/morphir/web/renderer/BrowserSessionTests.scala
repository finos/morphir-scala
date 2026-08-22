package morphir.web.renderer

import kyo.*
import kyo.test.*

class BrowserSessionTests extends Test[Any]:

  private final class FakeFetch(result: Result[Throwable, FetchJsonRpcTransport.Response])
      extends FetchJsonRpcTransport.Fetch:
    var requests = List.empty[FetchJsonRpcTransport.Request]
    def post(request: FetchJsonRpcTransport.Request)(using
        Frame
    ): Result[Throwable, FetchJsonRpcTransport.Response] < Async = Async.defer {
      requests = requests :+ request
      result
    }

  "BrowserSession" - {

    "exchanges a launch value once through same-origin JSON fetch" in {
      val fetch  = FakeFetch(Result.succeed(FetchJsonRpcTransport.Response(204, "")))
      val launch = "launch_value-123"
      BrowserSession.exchange(launch, fetch).map { _ =>
        val request = fetch.requests.head
        assert(fetch.requests.size == 1)
        assert(request.endpoint == "/api/session/exchange")
        assert(request.contentType == "application/json")
        assert(request.credentials == "same-origin")
        assert(request.body == s"""{"launch":"$launch"}""")
      }
    }

    "maps network failure to Closed without retaining the launch value" in {
      val launch = "launch_secret_value"
      val fetch  = FakeFetch(Result.fail(new RuntimeException("offline")))
      Abort.run[Closed](BrowserSession.exchange(launch, fetch)).map {
        case Result.Failure(error) => assert(!error.getMessage.contains(launch))
        case _                     => assert(false)
      }
    }
  }
end BrowserSessionTests
