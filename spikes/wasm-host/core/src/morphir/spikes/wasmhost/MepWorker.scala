package morphir.spikes.wasmhost

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}
import kyo.*

final case class MepOptions(guest: String = "", input: String = "", outputLimit: Int = 1048576, framed: Boolean = false)

/** NDJSON transport for the experiment. Reads are bounded before UTF-8 decoding or JSON allocation. */
private[wasmhost] object MepWorker:
  def utf8(bytes: Array[Byte]): String = StandardCharsets.UTF_8.newDecoder()
    .onMalformedInput(CodingErrorAction.REPORT).onUnmappableCharacter(CodingErrorAction.REPORT)
    .decode(ByteBuffer.wrap(bytes)).toString

  private def line(input: InputStream, limit: Int = 1048576): Maybe[Array[Byte]] =
    val output = ByteArrayOutputStream()
    @annotation.tailrec
    def loop(): Maybe[Array[Byte]] =
      input.read() match
        case -1    => if output.size() == 0 then Absent else Present(output.toByteArray)
        case 10    => Present(output.toByteArray)
        case value =>
          require(output.size() < limit, "input line exceeds its byte bound")
          output.write(value)
          loop()
    loop()

  private def frame(input: InputStream): Maybe[Array[Byte]] =
    var contentLength = -1
    @annotation.tailrec
    def headers(total: Int): Maybe[Array[Byte]] = line(input, 8192 - total) match
      case Absent =>
        require(total == 0, "truncated frame header")
        Absent
      case Present(bytes) =>
        require(total + bytes.length <= 8192, "frame headers exceed 8 KiB")
        val text = String(bytes, StandardCharsets.US_ASCII).trim
        if text.isEmpty then
          require(contentLength >= 0 && contentLength <= 1048576, "invalid or excessive Content-Length")
          val body = input.readNBytes(contentLength)
          require(body.length == contentLength, "truncated frame body")
          Present(body)
        else
          if text.toLowerCase(java.util.Locale.ROOT).startsWith("content-length:") then
            require(contentLength == -1, "duplicate Content-Length")
            contentLength = text.drop(15).trim.toInt
          headers(total + bytes.length + 1)
    headers(0)

  def run(backend: Backend, options: MepOptions): Unit < (Async & Sync) =
    require(options.guest.nonEmpty && Files.isRegularFile(Path.of(options.guest)), "--guest must name a file")
    require(options.outputLimit > 0 && options.outputLimit <= 1048576, "response bound must be 1..1048576")
    Scope.run {
      Scope.acquire(Sync.defer(backend.open(Path.of(options.guest), options.outputLimit))).map { session =>
        def serve(input: InputStream): Unit =
          @annotation.tailrec
          def loop(): Unit = (if options.framed then frame(input) else line(input)) match
            case Absent         => ()
            case Present(bytes) =>
              val requestText = utf8(bytes)
              val request     = ujson.read(requestText)
              val method      = request.obj.get("method").map(_.str).getOrElse("")
              if method == "morphir.exit" then
                if session.abi == "morphir-bytes-v1" then
                  val reply = session.handle(bytes)
                  require(reply.status == 0 && reply.bytes.isEmpty, "byte guest did not exit")
              else
                if request.obj.contains("id") || session.abi == "morphir-bytes-v1" then
                  val notification = !request.obj.contains("id")
                  val reply        = session.handle(bytes)
                  require(!notification || reply.status <= 0, "guest emitted a response to a notification")
                  require(
                    reply.status > 0 ||
                      ((reply.status == -2 || (reply.status == 0 && session.abi == "extism-kernel-v1")) &&
                        notification),
                    s"guest handle status ${reply.status} for this request"
                  )
                  if reply.status > 0 then
                    val responseText = utf8(reply.bytes)
                    val _            = ujson.read(responseText)
                    require(
                      JsonIdentity.extract(responseText) == JsonIdentity.extract(requestText),
                      "response identity mismatch"
                    )
                    if options.framed then
                      System.out.write(
                        s"Content-Length: ${reply.bytes.length}\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
                      )
                      System.out.write(reply.bytes)
                      System.out.flush()
                    else println(responseText)
                loop()
          loop()
        Sync.defer {
          if options.input.isEmpty then serve(System.in)
          else
            val input = Files.newInputStream(Path.of(options.input))
            try serve(input)
            finally input.close()
        }
      }
    }
