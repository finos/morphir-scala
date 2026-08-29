package morphir.langkit.elm.compiler.mep

import annotation.tailrec
import java.io.{InputStream, OutputStream, PrintStream}
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets.UTF_8
import kyo.*

object MepProcess:
  def run(
      input: InputStream,
      output: OutputStream,
      error: PrintStream,
      provider: ProviderMetadata
  ): Int =
    val readBuffer = new Array[Byte](8192)
    val decoder    = MepFrameCodec.decoder()

    @tailrec
    def dispatchFrames(frames: Vector[Array[Byte]], index: Int, session: MepSession): MepSession =
      if index >= frames.size || session.state == SessionState.Stopped then session
      else
        val transition = decodeUtf8(frames(index)).fold(_ => session.parseError, session.handle)
        transition.response.foreach { response =>
          output.write(MepFrameCodec.encodeJson(response))
          output.flush()
        }
        dispatchFrames(frames, index + 1, transition.session)

    @tailrec
    def loop(session: MepSession): Int =
      val count = input.read(readBuffer)
      if count < 0 then
        decoder.finish match
          case Result.Failure(frameError) =>
            error.println(frameError.message)
            1
          case Result.Success(_)   => 0
          case Result.Panic(cause) =>
            error.println(cause.getMessage)
            1
      else
        val decoded = decoder.feed(readBuffer.take(count))
        val next    = dispatchFrames(decoded.frames, 0, session)
        decoded.error match
          case Present(frameError) =>
            error.println(frameError.message)
            1
          case Absent if next.state == SessionState.Stopped => 0
          case Absent                                       => loop(next)

    loop(MepSession.loaded(provider))

  private def decodeUtf8(bytes: Array[Byte]): Either[Unit, String] =
    try
      val decoder = UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch case _: java.nio.charset.CharacterCodingException => Left(())
