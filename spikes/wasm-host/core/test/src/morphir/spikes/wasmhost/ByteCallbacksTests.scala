package morphir.spikes.wasmhost

import kyo.test.*

class ByteCallbacksTests extends Test[Any]:
  "byte callback boundary" - {
    "reads bytes unsigned and rejects offsets outside the request" in {
      val callbacks = ByteCallbacks(3)
      callbacks.begin(Array[Byte](-1, 0))
      assert(callbacks.inputByte(0) == 255)
      assert(callbacks.inputByte(1) == 0)
      assert(rejected(callbacks.inputByte(-1)))
      assert(rejected(callbacks.inputByte(2)))
    }
    "enforces output bounds before accumulating another byte" in {
      val callbacks = ByteCallbacks(1)
      callbacks.outputByte(255)
      assert(rejected(callbacks.outputByte(0)))
      assert(callbacks.finish(1).bytes.toSeq == Seq[Byte](-1))
    }
    "rejects non-byte output and incorrect positive response length" in {
      val callbacks = ByteCallbacks(3)
      assert(rejected(callbacks.outputByte(256)))
      assert(rejected(callbacks.outputByte(-1)))
      callbacks.outputByte(42)
      assert(rejected(callbacks.finish(2)))
    }
    "copies accepted output and isolates requests and sessions" in {
      val first = ByteCallbacks(3)
      first.begin(Array[Byte](1))
      first.outputByte(42)
      val accepted = first.finish(1).bytes
      first.begin(Array[Byte](2))
      first.outputByte(43)
      val other = ByteCallbacks(3)
      assert(accepted.toSeq == Seq[Byte](42))
      assert(first.inputByte(0) == 2)
      assert(other.inputLength == 0)
      assert(other.finish(0).bytes.isEmpty)
    }
  }

  private def rejected(body: => Any): Boolean =
    try
      val _ = body
      false
    catch case _: IllegalArgumentException => true
