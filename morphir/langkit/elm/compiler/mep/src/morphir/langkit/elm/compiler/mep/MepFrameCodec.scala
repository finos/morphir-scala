package morphir.langkit.elm.compiler.mep

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction}
import kyo.*

final case class MepFrameFeedOutcome(frames: Vector[Array[Byte]], error: Maybe[MepFrameError])

final case class MepFrameError(message: String) derives CanEqual

private[mep] sealed trait MepFrameReadState
private[mep] final case class ReadingHeader(buffer: Array[Byte], length: Int) extends MepFrameReadState
private[mep] final case class ReadingBody(buffer: Array[Byte], received: Int) extends MepFrameReadState

/** A non-thread-safe, single-owner streaming frame decoder. */
final class MepFrameDecoder private[mep] (
    private val headerScratch: Array[Byte],
    val maxPayloadBytes: Int,
    val maxHeaderBytes: Int,
    private var allocations: Int
):
  private var currentState  = ReadingHeader(headerScratch, length = 0): MepFrameReadState
  private var terminalError = Option.empty[MepFrameError]

  private[mep] def state: MepFrameReadState = currentState

  private[mep] def bodyAllocationCount: Int = allocations

  private[mep] def headerAllocationCount: Int = 1

  private[mep] def resetHeader(): ReadingHeader = ReadingHeader(headerScratch, length = 0)

  private[mep] def update(state: MepFrameReadState, bodyAllocationCount: Int): Unit =
    currentState = state
    allocations = bodyAllocationCount

  private[mep] def markTerminal(error: MepFrameError): Unit = terminalError = Some(error)

  def feed(bytes: Array[Byte]): MepFrameFeedOutcome =
    terminalError match
      case Some(error) => MepFrameFeedOutcome(Vector.empty, Present(error))
      case None        => MepFrameCodec.feed(this, bytes)

  def finish: Result[MepFrameError, Unit] =
    terminalError match
      case Some(error) => Result.fail(error)
      case None        =>
        state match
          case ReadingHeader(_, 0) => Result.succeed(())
          case _: ReadingHeader    => Result.fail(MepFrameError("truncated frame header"))
          case _: ReadingBody      => Result.fail(MepFrameError("truncated frame body"))

  private[mep] def hasAllocatedBody: Boolean = state.isInstanceOf[ReadingBody]

object MepFrameCodec:
  val DefaultMaxPayloadBytes: Int = 64 * 1024 * 1024
  val DefaultMaxHeaderBytes: Int  = 64 * 1024

  def decoder(
      maxPayloadBytes: Int = DefaultMaxPayloadBytes,
      maxHeaderBytes: Int = DefaultMaxHeaderBytes
  ): MepFrameDecoder =
    require(maxPayloadBytes >= 0, "maxPayloadBytes must be non-negative")
    require(maxHeaderBytes >= 0, "maxHeaderBytes must be non-negative")
    require(maxHeaderBytes <= Int.MaxValue - 4, "maxHeaderBytes cannot include delimiter lookahead")
    new MepFrameDecoder(new Array[Byte](maxHeaderBytes + 4), maxPayloadBytes, maxHeaderBytes, allocations = 0)

  def encode(body: Array[Byte]): Array[Byte] =
    s"Content-Length: ${body.length}\r\n\r\n".getBytes(UTF_8) ++ body

  def encodeJson(json: String): Array[Byte] = encode(json.getBytes(UTF_8))

  private[mep] def feed(
      decoder: MepFrameDecoder,
      input: Array[Byte]
  ): MepFrameFeedOutcome =
    var state       = decoder.state
    var allocations = decoder.bodyAllocationCount
    var offset      = 0
    var failure     = Option.empty[MepFrameError]
    val frames      = Vector.newBuilder[Array[Byte]]

    while offset < input.length && failure.isEmpty do
      state match
        case header @ ReadingHeader(buffer, length) =>
          if length >= buffer.length then
            failure = Some(MepFrameError(s"header exceeds ${decoder.maxHeaderBytes} bytes"))
          else
            buffer(length) = input(offset)
            offset += 1
            val nextLength = length + 1
            delimiterLengthAtEnd(buffer, nextLength) match
              case Some(delimiterLength) =>
                val headerLength = nextLength - delimiterLength
                if headerLength > decoder.maxHeaderBytes then
                  failure = Some(MepFrameError(s"header exceeds ${decoder.maxHeaderBytes} bytes"))
                else
                  parseLength(buffer, headerLength) match
                    case Left(error)                                               => failure = Some(error)
                    case Right(bodyLength) if bodyLength > decoder.maxPayloadBytes =>
                      failure = Some(MepFrameError(s"payload exceeds ${decoder.maxPayloadBytes} bytes"))
                    case Right(bodyLength) =>
                      val body = new Array[Byte](bodyLength)
                      allocations += 1
                      if bodyLength == 0 then
                        frames += body
                        state = decoder.resetHeader()
                      else state = ReadingBody(body, received = 0)
              case None =>
                if nextLength - pendingDelimiterBytes(buffer, nextLength) > decoder.maxHeaderBytes then
                  failure = Some(MepFrameError(s"header exceeds ${decoder.maxHeaderBytes} bytes"))
                else state = header.copy(length = nextLength)

        case body @ ReadingBody(buffer, received) =>
          val copied = Math.min(buffer.length - received, input.length - offset)
          java.lang.System.arraycopy(input, offset, buffer, received, copied)
          offset += copied
          val nextReceived = received + copied
          if nextReceived == buffer.length then
            frames += buffer
            state = decoder.resetHeader()
          else state = body.copy(received = nextReceived)

    decoder.update(state, allocations)
    failure.foreach(decoder.markTerminal)
    MepFrameFeedOutcome(frames.result(), Maybe.fromOption(failure))

  private def delimiterLengthAtEnd(bytes: Array[Byte], length: Int): Option[Int] =
    if length >= 4 && bytes(length - 4) == 13 && bytes(length - 3) == 10 && bytes(length - 2) == 13 &&
      bytes(length - 1) == 10
    then Some(4)
    else if length >= 2 && bytes(length - 2) == 10 && bytes(length - 1) == 10 then Some(2)
    else None

  private def pendingDelimiterBytes(bytes: Array[Byte], length: Int): Int =
    if length >= 3 && bytes(length - 3) == 13 && bytes(length - 2) == 10 && bytes(length - 1) == 13 then 3
    else if length >= 2 && bytes(length - 2) == 13 && bytes(length - 1) == 10 then 2
    else if length >= 1 && (bytes(length - 1) == 13 || bytes(length - 1) == 10) then 1
    else 0

  private def parseLength(header: Array[Byte], length: Int): Either[MepFrameError, Int] =
    decodeHeader(header, length).flatMap { text =>
      val lines   = text.split("\\r?\\n").toVector
      val lengths = lines.flatMap { line =>
        line.indexOf(':') match
          case -1                                                                        => Vector.empty
          case separator if line.take(separator).trim.equalsIgnoreCase("content-length") =>
            Vector(line.drop(separator + 1).trim)
          case _ => Vector.empty
      }
      val hasNonAsciiName = lines.exists { line =>
        val separator = line.indexOf(':')
        separator >= 0 && line.take(separator).exists(_ > 127)
      }
      if hasNonAsciiName then Left(MepFrameError("non-ASCII frame header"))
      else if lines.exists(line => line.nonEmpty && !line.contains(':')) then Left(MepFrameError("invalid header line"))
      else if lengths.isEmpty then Left(MepFrameError("missing Content-Length"))
      else if lengths.size > 1 then Left(MepFrameError("duplicate Content-Length"))
      else
        lengths match
          case Vector(value) =>
            Option.when(value.nonEmpty && value.forall(character => character >= '0' && character <= '9'))(value)
              .flatMap(_.toIntOption)
              .toRight(MepFrameError("invalid Content-Length"))
          case _ => Left(MepFrameError("invalid Content-Length"))
    }

  private def decodeHeader(header: Array[Byte], length: Int): Either[MepFrameError, String] =
    try
      val decoder = UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      Right(decoder.decode(ByteBuffer.wrap(header, 0, length)).toString)
    catch case _: CharacterCodingException => Left(MepFrameError("invalid header encoding"))
