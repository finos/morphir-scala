package morphir.langkit.elm.compiler.mep

import java.nio.charset.StandardCharsets.UTF_8
import kyo.*
import kyo.test.*

class MepFrameCodecTests extends Test[Any]:

  "MepFrameCodec" - {
    "rejects a negative payload limit" in {
      val rejected =
        try
          MepFrameCodec.decoder(maxPayloadBytes = -1)
          false
        catch case _: IllegalArgumentException => true

      assert(rejected)
    }

    "rejects a negative header limit" in {
      val rejected =
        try
          MepFrameCodec.decoder(maxHeaderBytes = -1)
          false
        catch case _: IllegalArgumentException => true

      assert(rejected)
    }

    "rejects a header limit that cannot include delimiter lookahead" in {
      val rejected =
        try
          MepFrameCodec.decoder(maxHeaderBytes = Int.MaxValue)
          false
        catch case _: IllegalArgumentException => true

      assert(rejected)
    }

    "decodes a frame split across arbitrary input fragments" in {
      val body    = "{\"jsonrpc\":\"2.0\",\"id\":1}".getBytes(UTF_8)
      val frame   = MepFrameCodec.encode(body)
      val decoder = MepFrameCodec.decoder()

      val first  = decoder.feed(frame.take(7))
      val second = decoder.feed(frame.slice(7, 23))
      val third  = decoder.feed(frame.drop(23))

      assert(first.frames.isEmpty)
      assert(second.frames.isEmpty)
      assert(third.frames.map(_.toSeq) == Vector(body.toSeq))
    }

    "allocates one bounded body only after a validated fragmented header" in {
      val initial = MepFrameCodec.decoder(maxPayloadBytes = 8, maxHeaderBytes = 32)
      initial.feed("Content-Length: 6\r\n".getBytes(UTF_8))

      assert(initial.bodyAllocationCount == 0)
      assert(!initial.hasAllocatedBody)

      initial.feed("\r\nab".getBytes(UTF_8))
      assert(initial.bodyAllocationCount == 1)
      assert(initial.hasAllocatedBody)

      initial.feed("cd".getBytes(UTF_8))
      val complete = initial.feed("ef".getBytes(UTF_8))

      assert(initial.bodyAllocationCount == 1)
      assert(complete.frames.map(bytes => String(bytes, UTF_8)) == Vector("abcdef"))
    }

    "keeps one decoder owner across fragmented feeds" in {
      val decoder = MepFrameCodec.decoder(maxPayloadBytes = 8, maxHeaderBytes = 32)

      decoder.feed("Content-Length: 2\r\n\r\n".getBytes(UTF_8))
      val completed = decoder.feed("{}".getBytes(UTF_8))

      assert(completed.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "reuses one header scratch buffer across sustained small frames" in {
      val decoder = MepFrameCodec.decoder(maxPayloadBytes = 8, maxHeaderBytes = 32)
      val outcome = decoder.feed(MepFrameCodec.encodeJson("{}") ++ MepFrameCodec.encodeJson("[]"))

      assert(outcome.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}", "[]"))
      assert(decoder.headerAllocationCount == 1)
    }

    "rejects duplicate Content-Length headers" in {
      val bytes = "Content-Length: 2\r\ncontent-length: 2\r\n\r\n{}".getBytes(UTF_8)

      assert(MepFrameCodec.decoder().feed(bytes).error == Present(MepFrameError("duplicate Content-Length")))
    }

    "decodes coalesced frames in one feed" in {
      val first  = "{}".getBytes(UTF_8)
      val second = "[]".getBytes(UTF_8)
      val result = MepFrameCodec.decoder().feed(MepFrameCodec.encode(first) ++ MepFrameCodec.encode(second))

      assert(result.frames.map(_.toSeq) == Vector(first.toSeq, second.toSeq))
    }

    "retains completed frames when a trailing frame is malformed" in {
      val valid     = "{\"valid\":true}".getBytes(UTF_8)
      val malformed = "Content-Length: nope\r\n\r\n".getBytes(UTF_8)

      val outcome = MepFrameCodec.decoder().feed(MepFrameCodec.encode(valid) ++ malformed)

      assert(outcome.frames.map(_.toSeq) == Vector(valid.toSeq))
      assert(outcome.error == Present(MepFrameError("invalid Content-Length")))
    }

    "stays terminal after a framing error" in {
      val decoder = MepFrameCodec.decoder()
      val error   = decoder
        .feed("Content-Length: nope\r\n\r\n".getBytes(UTF_8))
        .error
        .getOrElse(throw AssertionError("expected a framing error"))

      val afterError = decoder.feed(MepFrameCodec.encodeJson("{}"))

      assert(afterError.frames.isEmpty)
      assert(afterError.error == Present(error))
    }

    "produces the same completed frames and error across a chunk boundary" in {
      val valid     = MepFrameCodec.encodeJson("{}")
      val malformed = "Content-Length: nope\r\n\r\n".getBytes(UTF_8)
      val combined  = MepFrameCodec.decoder().feed(valid ++ malformed)
      val split     = MepFrameCodec.decoder()
      val first     = split.feed(valid)
      val second    = split.feed(malformed)

      assert(combined.frames.map(_.toSeq) == (first.frames ++ second.frames).map(_.toSeq))
      assert(combined.error == second.error)
    }

    "accepts LF-only frame headers" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: 2\n\n{}".getBytes(UTF_8))

      assert(result.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "uses UTF-8 byte length rather than character count" in {
      val frame = MepFrameCodec.encodeJson("{\"value\":\"λ\"}")

      assert(String(frame, UTF_8) == "Content-Length: 14\r\n\r\n{\"value\":\"λ\"}")
    }

    "rejects a missing Content-Length header" in {
      val result = MepFrameCodec.decoder().feed("Content-Type: application/json\r\n\r\n{}".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("missing Content-Length")))
    }

    "rejects a nonempty header line without a colon" in {
      val result = MepFrameCodec.decoder().feed("invalid\r\nContent-Length: 2\r\n\r\n{}".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("invalid header line")))
    }

    "rejects malformed UTF-8 in frame headers" in {
      val malformed = Array(0xc3.toByte) ++ ": value\r\nContent-Length: 2\r\n\r\n{}".getBytes(UTF_8)

      val result = MepFrameCodec.decoder().feed(malformed)

      assert(result.error == Present(MepFrameError("invalid header encoding")))
    }

    "rejects non-ASCII frame header names" in {
      val result = MepFrameCodec.decoder().feed("X-λ: value\r\nContent-Length: 2\r\n\r\n{}".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("non-ASCII frame header")))
    }

    "rejects a malformed Content-Length value" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: 2.5\r\n\r\n{}".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("invalid Content-Length")))
    }

    "rejects non-ASCII Content-Length digits" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: ٢\r\n\r\n{}".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("invalid Content-Length")))
    }

    "rejects a signed Content-Length value" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: +2\r\n\r\n{}".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("invalid Content-Length")))
    }

    "rejects a negative Content-Length value" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: -1\r\n\r\n".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("invalid Content-Length")))
    }

    "rejects an oversized length before receiving or allocating its body" in {
      val result = MepFrameCodec.decoder(maxPayloadBytes = 4).feed("Content-Length: 5\r\n\r\n".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("payload exceeds 4 bytes")))
    }

    "bounds the frame header" in {
      val result = MepFrameCodec.decoder(maxHeaderBytes = 4).feed("Conte".getBytes(UTF_8))

      assert(result.error == Present(MepFrameError("header exceeds 4 bytes")))
    }

    "accepts an exact-limit header when a CRLF delimiter arrives fragmented" in {
      val maxHeaderBytes = 32
      val prefix         = "Content-Length: 2\r\nX:"
      val header         = prefix + ("a" * (maxHeaderBytes - prefix.length))
      val fragments      = Vector("\r", "\n", "\r", "\n{}")
      val decoder        = MepFrameCodec.decoder(maxHeaderBytes = maxHeaderBytes)
      decoder.feed(header.getBytes(UTF_8))
      val result = fragments.foldLeft(MepFrameFeedOutcome(Vector.empty, Absent)) { case (_, fragment) =>
        decoder.feed(fragment.getBytes(UTF_8))
      }

      assert(result.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "accepts an exact-limit header when an LF delimiter arrives fragmented" in {
      val maxHeaderBytes = 32
      val prefix         = "Content-Length: 2\nX:"
      val header         = prefix + ("a" * (maxHeaderBytes - prefix.length))
      val decoder        = MepFrameCodec.decoder(maxHeaderBytes = maxHeaderBytes)
      decoder.feed(header.getBytes(UTF_8))
      decoder.feed("\n".getBytes(UTF_8))
      val result = decoder.feed("\n{}".getBytes(UTF_8))

      assert(result.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "reports a truncated body at EOF" in {
      val decoder = MepFrameCodec.decoder()
      decoder.feed("Content-Length: 4\r\n\r\n{}".getBytes(UTF_8))

      assert(decoder.finish == Result.fail(MepFrameError("truncated frame body")))
    }

    "reports a truncated header at EOF" in {
      val decoder = MepFrameCodec.decoder()
      decoder.feed("Content-Len".getBytes(UTF_8))

      assert(decoder.finish == Result.fail(MepFrameError("truncated frame header")))
    }
  }
