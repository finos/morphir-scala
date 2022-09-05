package morphir.mir

enum Fqn:
  case None
  case Module(id:String)
  case Member(module:Module, spec:Spec)

  final def show: String = Show(this)
  final def isModule: Boolean = this match
    case Module(_) => true
    case _         => false



