package morphir.util

case object UnreachableException                   extends Exception
final case class UnsupportedException(msg: String) extends Exception
