package morphir.web.renderer

import kyo.*

object BrowserSession:

  private final case class ExchangeRequest(launch: String) derives Schema:
    override def toString: String = "ExchangeRequest(<redacted>)"

  def exchange(
      launch: String,
      endpoint: String = "/api/session/exchange"
  )(using Frame): Unit < (Async & Abort[Closed]) =
    exchange(launch, FetchJsonRpcTransport.LiveFetch, endpoint)

  private[renderer] def exchange(
      launch: String,
      fetch: FetchJsonRpcTransport.Fetch
  )(using Frame): Unit < (Async & Abort[Closed]) =
    exchange(launch, fetch, "/api/session/exchange")

  private[renderer] def exchange(
      launch: String,
      fetch: FetchJsonRpcTransport.Fetch,
      endpoint: String
  )(using frame: Frame): Unit < (Async & Abort[Closed]) =
    val request = FetchJsonRpcTransport.Request(
      endpoint,
      Json.encode(ExchangeRequest(launch)),
      "application/json",
      "same-origin"
    )
    fetch.post(request).map {
      case Result.Success(FetchJsonRpcTransport.Response(204, _)) => ()
      case _                                                      => Abort.fail(new Closed("BrowserSession", frame))
    }
end BrowserSession
