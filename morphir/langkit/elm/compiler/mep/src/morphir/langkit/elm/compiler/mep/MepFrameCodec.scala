package morphir.langkit.elm.compiler.mep

import java.nio.charset.StandardCharsets.UTF_8

final case class DecodedFrames(decoder: MepFrameDecoder, frames: Vector[Array[Byte]])

final case class MepFrameError(message: String) derives CanEqual

final case class MepFrameDecoder private[mep] (buffer: Vector[Byte], maxPayloadBytes: Int, maxHeaderBytes: Int):
  def feed(bytes: Array[Byte]): Either[MepFrameError, DecodedFrames] =
    MepFrameCodec.decodeAvailable(buffer ++ bytes, Vector.empty, maxPayloadBytes, maxHeaderBytes)

  def finish: Either[MepFrameError, Unit] =
    if buffer.isEmpty then Right(())
    else if MepFrameCodec.hasCompleteHeader(buffer) then Left(MepFrameError("truncated frame body"))
    else Left(MepFrameError("truncated frame header"))

object MepFrameCodec:
  val DefaultMaxPayloadBytes: Int = 64 * 1024 * 1024
  val DefaultMaxHeaderBytes: Int  = 64 * 1024

  def decoder(
      maxPayloadBytes: Int = DefaultMaxPayloadBytes,
      maxHeaderBytes: Int = DefaultMaxHeaderBytes
  ): MepFrameDecoder =
    MepFrameDecoder(Vector.empty, maxPayloadBytes, maxHeaderBytes)

  def encode(body: Array[Byte]): Array[Byte] =
    s"Content-Length: ${body.length}\r\n\r\n".getBytes(UTF_8) ++ body

  def encodeJson(json: String): Array[Byte] = encode(json.getBytes(UTF_8))

  private[mep] def decodeAvailable(
      bytes: Vector[Byte],
      decoded: Vector[Array[Byte]],
      maxPayloadBytes: Int,
      maxHeaderBytes: Int
  ): Either[MepFrameError, DecodedFrames] =
    headerEnd(bytes) match
      case None if bytes.length - pendingDelimiterBytes(bytes) > maxHeaderBytes =>
        Left(MepFrameError(s"header exceeds $maxHeaderBytes bytes"))
      case None => Right(DecodedFrames(MepFrameDecoder(bytes, maxPayloadBytes, maxHeaderBytes), decoded))
      case Some((headerLength, delimiterLength)) =>
        if headerLength > maxHeaderBytes then Left(MepFrameError(s"header exceeds $maxHeaderBytes bytes"))
        else
          parseLength(bytes.take(headerLength)) match
            case Left(error)                                       => Left(error)
            case Right(bodyLength) if bodyLength > maxPayloadBytes =>
              Left(MepFrameError(s"payload exceeds $maxPayloadBytes bytes"))
            case Right(bodyLength) =>
              val bodyStart = headerLength + delimiterLength
              if bytes.length - bodyStart < bodyLength then
                Right(DecodedFrames(MepFrameDecoder(bytes, maxPayloadBytes, maxHeaderBytes), decoded))
              else
                val body      = bytes.slice(bodyStart, bodyStart + bodyLength).toArray
                val remaining = bytes.drop(bodyStart + bodyLength)
                decodeAvailable(remaining, decoded :+ body, maxPayloadBytes, maxHeaderBytes)

  private def headerEnd(bytes: Vector[Byte]): Option[(Int, Int)] =
    val crlf = bytes.indexOfSlice(Vector[Byte](13, 10, 13, 10))
    val lf   = bytes.indexOfSlice(Vector[Byte](10, 10))
    (crlf, lf) match
      case (-1, -1)         => None
      case (-1, n)          => Some(n -> 2)
      case (n, -1)          => Some(n -> 4)
      case (a, b) if a <= b => Some(a -> 4)
      case (_, b)           => Some(b -> 2)

  private[mep] def hasCompleteHeader(bytes: Vector[Byte]): Boolean = headerEnd(bytes).nonEmpty

  private def pendingDelimiterBytes(bytes: Vector[Byte]): Int =
    Vector(Vector[Byte](10), Vector[Byte](13), Vector[Byte](13, 10), Vector[Byte](13, 10, 13))
      .filter(bytes.endsWith)
      .map(_.length)
      .maxOption
      .getOrElse(0)

  private def parseLength(header: Vector[Byte]): Either[MepFrameError, Int] =
    val lines   = String(header.toArray, UTF_8).split("\\r?\\n").toVector
    val lengths = lines.flatMap { line =>
      line.indexOf(':') match
        case -1                                                                        => Vector.empty
        case separator if line.take(separator).trim.equalsIgnoreCase("content-length") =>
          Vector(line.drop(separator + 1).trim)
        case _ => Vector.empty
    }
    if lines.exists(line => line.nonEmpty && !line.contains(':')) then Left(MepFrameError("invalid header line"))
    else if lengths.isEmpty then Left(MepFrameError("missing Content-Length"))
    else if lengths.size > 1 then Left(MepFrameError("duplicate Content-Length"))
    else
      lengths match
        case Vector(value) =>
          Option.when(value.nonEmpty && value.forall(_.isDigit))(value)
            .flatMap(_.toIntOption)
            .toRight(MepFrameError("invalid Content-Length"))
        case _ => Left(MepFrameError("invalid Content-Length"))
