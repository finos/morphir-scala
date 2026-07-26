package morphir.langkit.elm.log

import kyo.*
import scribe.Logger

final class ScribeLogHandler(logger: Logger):

  val log: Log =
    Log(ScribeLogHandler.unsafe(logger, ScribeLogHandler.DefaultName))

object ScribeLogHandler:

  private val DefaultName = "morphir.langkit.scribe-log-handler"

  private def unsafe(logger: Logger, loggerName: String): Log.Unsafe =
    new Log.Unsafe:
      def level: Log.Level                   = Log.Level.trace
      def name: String                       = loggerName
      def withName(name: String): Log.Unsafe = unsafe(Logger(name), name)

      def trace(msg: => String)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.trace(msg)

      def trace(msg: => String, t: => Throwable)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.trace(msg, t)

      def debug(msg: => String)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.debug(msg)

      def debug(msg: => String, t: => Throwable)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.debug(msg, t)

      def info(msg: => String)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.info(msg)

      def info(msg: => String, t: => Throwable)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.info(msg, t)

      def warn(msg: => String)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.warn(msg)

      def warn(msg: => String, t: => Throwable)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.warn(msg, t)

      def error(msg: => String)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.error(msg)

      def error(msg: => String, t: => Throwable)(using frame: Frame, allow: AllowUnsafe): Unit =
        logger.error(msg, t)
