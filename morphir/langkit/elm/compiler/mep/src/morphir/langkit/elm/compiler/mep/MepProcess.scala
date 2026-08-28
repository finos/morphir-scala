package morphir.langkit.elm.compiler.mep

import java.io.{InputStream, OutputStream, PrintStream}
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets.UTF_8

object MepProcess:
  def run(
      input: InputStream,
      output: OutputStream,
      error: PrintStream,
      provider: ProviderMetadata
  ): Int =
    val readBuffer = new Array[Byte](8192)
    val decoder    = MepFrameCodec.decoder()
    var session    = MepSession.loaded(provider)
    var done       = false
    var exitCode   = 0

    while !done do
      val count = input.read(readBuffer)
      if count < 0 then
        decoder.finish match
          case Left(frameError) =>
            error.println(frameError.message)
            exitCode = 1
          case Right(_) => ()
        done = true
      else
        val decoded = decoder.feed(readBuffer.take(count))
        decoded.frames.iterator.takeWhile(_ => !done).foreach { frame =>
          val transition = decodeUtf8(frame).fold(_ => session.parseError, session.handle)
          session = transition.session
          transition.response.foreach { response =>
            output.write(MepFrameCodec.encodeJson(response))
            output.flush()
          }
          if session.state == SessionState.Stopped then done = true
        }
        decoded.error.foreach { frameError =>
          error.println(frameError.message)
          exitCode = 1
          done = true
        }
    exitCode

  private def decodeUtf8(bytes: Array[Byte]): Either[Unit, String] =
    try
      val decoder = UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch case _: java.nio.charset.CharacterCodingException => Left(())
