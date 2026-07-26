package morphir.langkit.elm.log

import kyo.*
import kyo.test.*
import scribe.{Level as ScribeLevel, Logger}
import scribe.handler.LogHandler

import scala.collection.mutable

class ScribeLogHandlerSpec extends Test[Any]:

  "ScribeLogHandler / InMemoryLogRecorder" - {

    "InMemoryLogRecorder captures every level emitted via Kyo Log" in {
      val recorder = InMemoryLogRecorder.unsafeMake()
      val program  =
        Log.let(InMemoryLogRecorder.layer(recorder)) {
          for
            _ <- Log.trace("trace-msg")
            _ <- Log.debug("debug-msg")
            _ <- Log.info("info-msg")
            _ <- Log.warn("warn-msg")
            _ <- Log.error("error-msg")
          yield ()
        }
      program.andThen(Log.flush).map { _ =>
        val events = recorder.snapshot()
        assert(events.map(_.message) == List("trace-msg", "debug-msg", "info-msg", "warn-msg", "error-msg"))
      }
    }

    "InMemoryLogRecorder preserves emission order" in {
      val recorder = InMemoryLogRecorder.unsafeMake()
      val program  =
        Log.let(InMemoryLogRecorder.layer(recorder)) {
          for
            _ <- Log.info("first")
            _ <- Log.info("second")
            _ <- Log.info("third")
          yield ()
        }
      program.andThen(Log.flush).map { _ =>
        val events = recorder.snapshot()
        assert(events.map(_.message) == List("first", "second", "third"))
      }
    }

    "InMemoryLogRecorder preserves derived names and shares its buffer" in {
      val recorder     = InMemoryLogRecorder.unsafeMake()
      val successName  = "krueger.tests.in-memory.primary"
      val distinctName = "krueger.tests.in-memory.secondary"
      val cause        = new IllegalStateException("in-memory-derived-cause")
      val program      =
        Log.let(InMemoryLogRecorder.layer(recorder)) {
          for
            success  <- Log.init(successName)
            _        <- Log.let(successName)(Log.info("primary-message"))
            distinct <- Log.init(distinctName)
            _        <- Log.let(distinctName)(Log.warn("distinct-message", cause))
            empty    <- Log.init("")
            _        <- Log.let("")(Log.error("empty-message"))
          yield List(success.name, distinct.name, empty.name)
        }

      for
        names <- program
        _     <- Log.flush
      yield
        assert(names == List(successName, distinctName, ""))
        assert(
          recorder.snapshot() == List(
            LogRecord(Log.Level.info, "primary-message"),
            LogRecord(Log.Level.warn, "distinct-message", Some(cause)),
            LogRecord(Log.Level.error, "empty-message")
          )
        )
    }

    "ScribeLogHandler does not throw on every level" in {
      val program =
        Log.let(ScribeLogLayer.default) {
          for
            _ <- Log.trace("trace-msg")
            _ <- Log.debug("debug-msg")
            _ <- Log.info("info-msg")
            _ <- Log.warn("warn-msg")
            _ <- Log.error("error-msg")
          yield ()
        }
      program.andThen(Log.flush).map(_ => succeed)
    }

    "ScribeLogHandler preserves requested names on derived loggers" in {
      val successName  = "krueger.tests.scribe.names.primary"
      val distinctName = "krueger.tests.scribe.names.secondary"
      val edgeName     = "krueger.tests.scribe.names.$"
      val requested    = List(successName, distinctName, edgeName)
      val program      =
        Log.let(ScribeLogLayer.forLogger(Logger.empty.orphan())) {
          for
            success  <- Log.init(successName)
            distinct <- Log.init(distinctName)
            edge     <- Log.init(edgeName)
            empty    <- Log.init("")
          yield (List(success.name, distinct.name, edge.name), empty.name)
        }

      program.map { case (names, emptyName) =>
        requested.flatMap(Logger.get).foreach(_.remove())
        assert(names == requested)
        assert(emptyName == "")
      }
    }

    "ScribeLogHandler routes derived loggers through named Scribe loggers" in {
      val routed       = mutable.ArrayBuffer.empty[(String, ScribeLevel, String)]
      val baseRouted   = mutable.ArrayBuffer.empty[String]
      val successName  = "krueger.tests.scribe.primary"
      val distinctName = "krueger.tests.scribe.secondary"
      val cause        = new IllegalStateException("scribe-derived-cause")

      def namedLogger(name: String): Logger =
        Logger.empty
          .orphan()
          .withHandler(LogHandler(ScribeLevel.Trace) { record =>
            routed.synchronized {
              routed += ((name, record.level, record.logOutput.plainText))
              ()
            }
          })
          .replace(Some(name))

      val configured = List(successName, distinctName, "").map(namedLogger)
      val baseLogger =
        Logger.empty
          .orphan()
          .withHandler(LogHandler(ScribeLevel.Trace) { record =>
            baseRouted.synchronized {
              baseRouted += record.logOutput.plainText
              ()
            }
          })

      val program =
        Log.let(ScribeLogLayer.forLogger(baseLogger)) {
          for
            _ <- Log.let(successName)(Log.info("primary-message"))
            _ <- Log.let(distinctName)(Log.warn("distinct-message", cause))
            _ <- Log.let("")(Log.error("empty-message"))
          yield ()
        }

      for
        _ <- program
        _ <- Log.flush
      yield
        val events     = routed.synchronized(routed.toList)
        val baseEvents = baseRouted.synchronized(baseRouted.toList)
        configured.foreach(_.remove())

        assert(baseEvents.isEmpty)
        assert(
          events.map(event => (event._1, event._2.name)) == List(
            (successName, "INFO"),
            (distinctName, "WARN"),
            ("", "ERROR")
          )
        )
        assert(events.head._3 == "primary-message")
        assert(events(1)._3.startsWith("distinct-message"))
        assert(events(1)._3.contains("scribe-derived-cause"))
        assert(events(2)._3 == "empty-message")
    }
  }
