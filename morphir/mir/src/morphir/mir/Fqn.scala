package morphir.mir

sealed abstract class Fqn:
  final def show:String = Show(this)

object Fqn:
  case object None extends Fqn
