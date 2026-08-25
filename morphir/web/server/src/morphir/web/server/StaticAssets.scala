package morphir.web.server

import java.io.ByteArrayOutputStream
import kyo.*

private[server] object StaticAssets:

  enum Rejection derives CanEqual:
    case BadPath, NotFound

  final case class Asset(bytes: Span[Byte], contentType: String, html: Boolean) derives CanEqual

  private final case class Entry(resource: String, contentType: String, html: Boolean)

  private val entries = Map(
    "/"                               -> Entry("morphir-web/index.html", "text/html; charset=utf-8", true),
    "/index.html"                     -> Entry("morphir-web/index.html", "text/html; charset=utf-8", true),
    "/assets/morphir-web.css"         -> Entry("morphir-web/assets/morphir-web.css", "text/css; charset=utf-8", false),
    "/assets/morphir-web-renderer.js" -> Entry(
      "morphir-web/assets/morphir-web-renderer.js",
      "text/javascript; charset=utf-8",
      false
    )
  )

  def load(path: String)(using Frame): Result[Rejection, Asset] < Sync =
    canonical(path) match
      case Result.Success(canonicalPath) =>
        entries.get(canonicalPath) match
          case Some(entry) => read(entry)
          case None        => Result.fail(Rejection.NotFound)
      case Result.Failure(error) => Result.fail(error)
      case Result.Panic(error)   => Result.panic(error)

  private def canonical(path: String): Result[Rejection, String] =
    if !path.startsWith("/") || path.contains('\\') || path.contains('\u0000') || path.contains("//") then
      Result.fail(Rejection.BadPath)
    else
      decodeRepeated(path, 0) match
        case Result.Success(decoded) if decoded != path     => Result.fail(Rejection.BadPath)
        case Result.Success(decoded) if suspicious(decoded) => Result.fail(Rejection.BadPath)
        case Result.Success(decoded)                        => Result.succeed(decoded)
        case Result.Failure(error)                          => Result.fail(error)
        case Result.Panic(error)                            => Result.panic(error)

  private def decodeRepeated(value: String, round: Int): Result[Rejection, String] =
    if round == 3 then
      if value.contains('%') then Result.fail(Rejection.BadPath)
      else Result.succeed(value)
    else
      decodeOnce(value) match
        case Result.Success(decoded) if decoded == value => Result.succeed(decoded)
        case Result.Success(decoded)                     => decodeRepeated(decoded, round + 1)
        case Result.Failure(error)                       => Result.fail(error)
        case Result.Panic(error)                         => Result.panic(error)

  private def decodeOnce(value: String): Result[Rejection, String] =
    val result = StringBuilder()
    var index  = 0
    while index < value.length do
      if value.charAt(index) != '%' then
        result.append(value.charAt(index))
        index += 1
      else if index + 2 >= value.length then return Result.fail(Rejection.BadPath)
      else
        val high = Character.digit(value.charAt(index + 1), 16)
        val low  = Character.digit(value.charAt(index + 2), 16)
        if high < 0 || low < 0 then return Result.fail(Rejection.BadPath)
        result.append(((high << 4) | low).toChar)
        index += 3
    Result.succeed(result.result())

  private def suspicious(path: String): Boolean =
    path.contains('\\') || path.contains('\u0000') || path.contains('%') ||
      path.split("/", -1).exists(segment => segment == "." || segment == "..")

  private def read(entry: Entry)(using Frame): Result[Rejection, Asset] < Sync = Sync.defer {
    val stream = StaticAssets.getClass.getClassLoader.getResourceAsStream(entry.resource)
    if stream == null then Result.fail(Rejection.NotFound)
    else
      try
        val output = ByteArrayOutputStream()
        stream.transferTo(output)
        Result.succeed(Asset(Span.fromUnsafe(output.toByteArray), entry.contentType, entry.html))
      finally stream.close()
  }
end StaticAssets
