package morphir

import kyo.*
import scala.util.control.NoStackTrace

/**
 * Root of Morphir's typed-error exception hierarchy, after Kyo's `KyoException`.
 *
 * Error ADTs extend this so a typed failure carried in `Result` or `Abort` is also a catchable JVM exception at
 * boundaries that demand one. `NoStackTrace` keeps construction cheap; errors are values first.
 *
 * Declared in package `morphir` (kyo-style flat namespace) though shipped from the buildkit core artifact, which sits
 * at the dependency bottom of the Kyo-based stack. This makes `morphir` a split package across future artifacts,
 * exactly as every kyo artifact contributes to package `kyo`; bead morphir-mww tracks the trade.
 */
abstract class MorphirException(message: String, cause: Maybe[Throwable] = Absent)
    extends Exception(message, cause.getOrElse(null))
    with NoStackTrace
