package morphir.spikes.wasmhost

import java.nio.file.Path
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import kyo.*

final case class SafetyOptions(guest: String = "", runaway: String = "")

private[wasmhost] object SafetyChecks:
  def run(backend: Backend, options: SafetyOptions): Unit =
    require(options.guest.nonEmpty, "--guest is required")
    val first = backend.open(Path.of(options.guest), 65536)
    try
      val second = backend.open(Path.of(options.guest), 65536)
      try
        val original = Array[Byte](0, -1, 42)
        val retained = first.handle(original).bytes
        @annotation.tailrec
        def loop(remaining: Int): Unit =
          if remaining > 0 then
            require(
              java.util.Arrays.equals(second.handle(Array[Byte](1, 2)).bytes, Array[Byte](1, 2)),
              "sessions exchanged callback state"
            )
            require(java.util.Arrays.equals(first.handle(original).bytes, original), "retained callback failed")
            if remaining % 10 == 0 then System.gc()
            loop(remaining - 1)
        loop(100)
        require(java.util.Arrays.equals(retained, original), "borrowed output escaped and changed after later calls")
      finally second.close()
    finally first.close()
    var cancellation = "unavailable; parent kill/reap required"
    if backend.supportsCancellation && options.runaway.nonEmpty then
      val session  = backend.open(Path.of(options.runaway), 65536)
      val started  = CountDownLatch(1)
      val finished = CountDownLatch(1)
      val outcome  = AtomicReference[Maybe[Throwable]](Absent)
      val thread   = Thread.ofPlatform().name("wasm-cancellation-proof").unstarted(() =>
        started.countDown()
        try
          val _ = session.handle(Array.emptyByteArray)
        catch case error: Throwable => outcome.set(Present(error))
        finally finished.countDown()
      )
      thread.start()
      require(started.await(1, TimeUnit.SECONDS), "native call did not start")
      Thread.sleep(100)
      val accepted = session.cancel()
      val returned = finished.await(5, TimeUnit.SECONDS)
      // On failure the enclosing worker must be killed; never free native state under a live downcall.
      if !returned then throw IllegalStateException("native cancellation did not return; worker kill required")
      thread.join()
      session.close()
      require(!thread.isAlive && outcome.get().isDefined, "cancelled call did not report failure")
      cancellation = s"returned-before-close; cancellation accepted=$accepted"
    println(ujson.Obj(
      "runtime"                  -> backend.name,
      "scope"                    -> "synthetic-safety-checks",
      "status"                   -> "pass",
      "retainedCallbacksUnderGc" -> 100,
      "copiedOutput"             -> true,
      "privateSessionState"      -> true,
      "nativeCancellation"       -> cancellation
    ).render())
