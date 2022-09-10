package morphir.sdk
import morphir.interop.extern
import scala.{Int => SInt}

@extern
object Int:
  opaque type Int = SInt

  private[sdk] def wrap(int: SInt): Int   = int
  private[sdk] def unwrap(int: Int): SInt = int
