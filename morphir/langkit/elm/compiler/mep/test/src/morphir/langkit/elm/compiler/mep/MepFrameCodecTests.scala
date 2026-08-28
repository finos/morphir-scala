package morphir.langkit.elm.compiler.mep

import java.nio.charset.StandardCharsets.UTF_8
import kyo.test.*

class MepFrameCodecTests extends Test[Any]:

  "MepFrameCodec" - {
    "decodes a frame split across arbitrary input fragments" in {
      val body  = "{\"jsonrpc\":\"2.0\",\"id\":1}".getBytes(UTF_8)
      val frame = MepFrameCodec.encode(body)

      val first  = MepFrameCodec.decoder().feed(frame.take(7))
      val second = first.toOption.get.decoder.feed(frame.slice(7, 23))
      val third  = second.toOption.get.decoder.feed(frame.drop(23))

      assert(first.toOption.get.frames.isEmpty)
      assert(second.toOption.get.frames.isEmpty)
      assert(third.toOption.get.frames.map(_.toSeq) == Vector(body.toSeq))
    }

    "allocates one bounded body only after a validated fragmented header" in {
      val initial       = MepFrameCodec.decoder(maxPayloadBytes = 8, maxHeaderBytes = 32)
      val partialHeader = initial.feed("Content-Length: 6\r\n".getBytes(UTF_8)).toOption.get.decoder

      assert(initial.bodyAllocationCount == 0)
      assert(!initial.hasAllocatedBody)
      assert(partialHeader.bodyAllocationCount == 0)
      assert(!partialHeader.hasAllocatedBody)

      val partialBody = partialHeader.feed("\r\nab".getBytes(UTF_8)).toOption.get.decoder
      assert(partialBody.bodyAllocationCount == 1)
      assert(partialBody.hasAllocatedBody)

      val moreBody = partialBody.feed("cd".getBytes(UTF_8)).toOption.get.decoder
      val complete = moreBody.feed("ef".getBytes(UTF_8)).toOption.get

      assert(moreBody.bodyAllocationCount == 1)
      assert(complete.decoder.bodyAllocationCount == 1)
      assert(complete.frames.map(bytes => String(bytes, UTF_8)) == Vector("abcdef"))
    }

    "rejects duplicate Content-Length headers" in {
      val bytes = "Content-Length: 2\r\ncontent-length: 2\r\n\r\n{}".getBytes(UTF_8)

      assert(MepFrameCodec.decoder().feed(bytes) == Left(MepFrameError("duplicate Content-Length")))
    }

    "decodes coalesced frames in one feed" in {
      val first  = "{}".getBytes(UTF_8)
      val second = "[]".getBytes(UTF_8)
      val result = MepFrameCodec.decoder().feed(MepFrameCodec.encode(first) ++ MepFrameCodec.encode(second))

      assert(result.toOption.get.frames.map(_.toSeq) == Vector(first.toSeq, second.toSeq))
    }

    "accepts LF-only frame headers" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: 2\n\n{}".getBytes(UTF_8))

      assert(result.toOption.get.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "uses UTF-8 byte length rather than character count" in {
      val frame = MepFrameCodec.encodeJson("{\"value\":\"λ\"}")

      assert(String(frame, UTF_8) == "Content-Length: 14\r\n\r\n{\"value\":\"λ\"}")
    }

    "rejects a missing Content-Length header" in {
      val result = MepFrameCodec.decoder().feed("Content-Type: application/json\r\n\r\n{}".getBytes(UTF_8))

      assert(result == Left(MepFrameError("missing Content-Length")))
    }

    "rejects a nonempty header line without a colon" in {
      val result = MepFrameCodec.decoder().feed("invalid\r\nContent-Length: 2\r\n\r\n{}".getBytes(UTF_8))

      assert(result == Left(MepFrameError("invalid header line")))
    }

    "rejects malformed UTF-8 in frame headers" in {
      val malformed = Array(0xc3.toByte) ++ ": value\r\nContent-Length: 2\r\n\r\n{}".getBytes(UTF_8)

      val result = MepFrameCodec.decoder().feed(malformed)

      assert(result == Left(MepFrameError("invalid header encoding")))
    }

    "rejects non-ASCII frame header names" in {
      val result = MepFrameCodec.decoder().feed("X-λ: value\r\nContent-Length: 2\r\n\r\n{}".getBytes(UTF_8))

      assert(result == Left(MepFrameError("non-ASCII frame header")))
    }

    "rejects a malformed Content-Length value" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: 2.5\r\n\r\n{}".getBytes(UTF_8))

      assert(result == Left(MepFrameError("invalid Content-Length")))
    }

    "rejects non-ASCII Content-Length digits" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: ٢\r\n\r\n{}".getBytes(UTF_8))

      assert(result == Left(MepFrameError("invalid Content-Length")))
    }

    "rejects a signed Content-Length value" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: +2\r\n\r\n{}".getBytes(UTF_8))

      assert(result == Left(MepFrameError("invalid Content-Length")))
    }

    "rejects a negative Content-Length value" in {
      val result = MepFrameCodec.decoder().feed("Content-Length: -1\r\n\r\n".getBytes(UTF_8))

      assert(result == Left(MepFrameError("invalid Content-Length")))
    }

    "rejects an oversized length before receiving or allocating its body" in {
      val result = MepFrameCodec.decoder(maxPayloadBytes = 4).feed("Content-Length: 5\r\n\r\n".getBytes(UTF_8))

      assert(result == Left(MepFrameError("payload exceeds 4 bytes")))
    }

    "bounds the frame header" in {
      val result = MepFrameCodec.decoder(maxHeaderBytes = 4).feed("Conte".getBytes(UTF_8))

      assert(result == Left(MepFrameError("header exceeds 4 bytes")))
    }

    "accepts an exact-limit header when a CRLF delimiter arrives fragmented" in {
      val maxHeaderBytes = 32
      val prefix         = "Content-Length: 2\r\nX:"
      val header         = prefix + ("a" * (maxHeaderBytes - prefix.length))
      val fragments      = Vector("\r", "\n", "\r", "\n{}")
      val result         =
        fragments.foldLeft(MepFrameCodec.decoder(maxHeaderBytes = maxHeaderBytes).feed(header.getBytes(UTF_8))) {
          case (Right(decoded), fragment) => decoded.decoder.feed(fragment.getBytes(UTF_8))
          case (left, _)                  => left
        }

      assert(result.toOption.get.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "accepts an exact-limit header when an LF delimiter arrives fragmented" in {
      val maxHeaderBytes = 32
      val prefix         = "Content-Length: 2\nX:"
      val header         = prefix + ("a" * (maxHeaderBytes - prefix.length))
      val first          = MepFrameCodec.decoder(maxHeaderBytes = maxHeaderBytes).feed(header.getBytes(UTF_8))
      val delimiterStart = first.toOption.get.decoder.feed("\n".getBytes(UTF_8))
      val result         = delimiterStart.toOption.get.decoder.feed("\n{}".getBytes(UTF_8))

      assert(result.toOption.get.frames.map(bytes => String(bytes, UTF_8)) == Vector("{}"))
    }

    "reports a truncated body at EOF" in {
      val partial = MepFrameCodec.decoder().feed("Content-Length: 4\r\n\r\n{}".getBytes(UTF_8)).toOption.get

      assert(partial.decoder.finish == Left(MepFrameError("truncated frame body")))
    }

    "reports a truncated header at EOF" in {
      val partial = MepFrameCodec.decoder().feed("Content-Len".getBytes(UTF_8)).toOption.get

      assert(partial.decoder.finish == Left(MepFrameError("truncated frame header")))
    }
  }
