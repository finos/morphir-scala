package morphir.langkit.elm.compiler.mep

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction}

final case class DecodedFrames(decoder: MepFrameDecoder, frames: Vector[Array[Byte]])

final case class MepFrameError(message: String) derives CanEqual

private[mep] sealed trait MepFrameReadState
private[mep] final case class ReadingHeader(buffer: Array[Byte], length: Int) extends MepFrameReadState
private[mep] final case class ReadingBody(buffer: Array[Byte], received: Int) extends MepFrameReadState

final case class MepFrameDecoder private[mep] (
    private[mep] val state: MepFrameReadState,
    maxPayloadBytes: Int,
    maxHeaderBytes: Int,
    private[mep] val bodyAllocationCount: Int
):
  def feed(bytes: Array[Byte]): Either[MepFrameError, DecodedFrames] = MepFrameCodec.feed(this, bytes)

  def finish: Either[MepFrameError, Unit] = state match
    case ReadingHeader(_, 0) => Right(())
    case _: ReadingHeader    => Left(MepFrameError("truncated frame header"))
    case _: ReadingBody      => Left(MepFrameError("truncated frame body"))

  private[mep] def hasAllocatedBody: Boolean = state.isInstanceOf[ReadingBody]

object MepFrameCodec:
  val DefaultMaxPayloadBytes: Int = 64 * 1024 * 1024
  val DefaultMaxHeaderBytes: Int  = 64 * 1024

  def decoder(
      maxPayloadBytes: Int = DefaultMaxPayloadBytes,
      maxHeaderBytes: Int = DefaultMaxHeaderBytes
  ): MepFrameDecoder =
    MepFrameDecoder(newHeader(maxHeaderBytes), maxPayloadBytes, maxHeaderBytes, bodyAllocationCount = 0)

  def encode(body: Array[Byte]): Array[Byte] =
    s"Content-Length: ${body.length}\r\n\r\n".getBytes(UTF_8) ++ body

  def encodeJson(json: String): Array[Byte] = encode(json.getBytes(UTF_8))

  private[mep] def feed(
      decoder: MepFrameDecoder,
      input: Array[Byte]
  ): Either[MepFrameError, DecodedFrames] =
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
                        state = newHeader(decoder.maxHeaderBytes)
                      else state = ReadingBody(body, received = 0)
              case None =>
                if nextLength - pendingDelimiterBytes(buffer, nextLength) > decoder.maxHeaderBytes then
                  failure = Some(MepFrameError(s"header exceeds ${decoder.maxHeaderBytes} bytes"))
                else state = header.copy(length = nextLength)

        case body @ ReadingBody(buffer, received) =>
          val copied = Math.min(buffer.length - received, input.length - offset)
          System.arraycopy(input, offset, buffer, received, copied)
          offset += copied
          val nextReceived = received + copied
          if nextReceived == buffer.length then
            frames += buffer
            state = newHeader(decoder.maxHeaderBytes)
          else state = body.copy(received = nextReceived)

    failure match
      case Some(error) => Left(error)
      case None        =>
        Right(
          DecodedFrames(
            MepFrameDecoder(state, decoder.maxPayloadBytes, decoder.maxHeaderBytes, allocations),
            frames.result()
          )
        )

  private def newHeader(maxHeaderBytes: Int): ReadingHeader =
    ReadingHeader(new Array[Byte](maxHeaderBytes + 4), length = 0)

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
