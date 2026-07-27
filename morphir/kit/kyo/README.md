# morphir-kit-kyo

A Kyo `Log` implementation backed by [scribe](https://github.com/outr/scribe), plus an in-memory recorder for
asserting on log output in tests.

Kyo's `Log` effect needs a handler to do anything. This module supplies two: one that forwards to scribe, and one that
collects records in memory.

## Logging through scribe

`ScribeLogLayer.default` uses scribe's root logger; `forLogger` takes a specific one.

```scala
import kyo.*
import morphir.kit.kyo.log.ScribeLogLayer

val program: Unit < (Sync & Abort[Nothing]) =
  Log.let(ScribeLogLayer.default) {
    Log.info("compiling") *> Log.warn("deprecated syntax")
  }
```

`ScribeLogHandler` is the handler itself, if you want to build the `Log` yourself rather than take the layer.

The handler reports at `Log.Level.trace` and leaves filtering to scribe, so level configuration is scribe's
concern rather than something to duplicate here. `withName` re-resolves through `scribe.Logger(name)`, so
Kyo's named-logger calls land on the correspondingly named scribe logger.

## Recording logs in tests

`InMemoryLogRecorder` captures each call as a `LogRecord(level, message, cause)`, which is usually easier to assert on
than parsing formatted output:

```scala
import kyo.*
import morphir.kit.kyo.log.{InMemoryLogRecorder, LogRecord}

val recorder = InMemoryLogRecorder.unsafeMake()

Log.let(InMemoryLogRecorder.layer(recorder)) {
  Log.info("parsed 3 modules")
}

recorder.snapshot()  // Seq(LogRecord(Log.Level.info, "parsed 3 modules", None))
recorder.clear()
```

`snapshot()` returns an immutable copy and `clear()` resets; both are synchronized, so a recorder can be shared across
concurrent effects.

## Artifact

`org.finos.morphir::morphir-kit-kyo` — JVM, Scala.js, and Scala Native.
