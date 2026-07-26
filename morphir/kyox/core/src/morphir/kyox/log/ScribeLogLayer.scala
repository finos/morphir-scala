package morphir.kyox.log

import kyo.*
import scribe.Logger

object ScribeLogLayer:

  val default: Log =
    new ScribeLogHandler(Logger.root).log

  def forLogger(logger: Logger): Log =
    new ScribeLogHandler(logger).log
