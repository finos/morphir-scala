package morphir.mir

enum Global:
  case None
  case Top(id: String)
  case Member(owner: Global, spec: Spec)

  final def isTop: Boolean = this.isInstanceOf[Global.Top]

  final def mangle: String = Mangle(this)

  final def member(spec: Spec): Global = this match
    case None             => throw new Exception("Global.None cannot have members.")
    case Top(id)          => Global.Member(this, spec)
    case Member(owner, _) => throw new Exception("Global.Member cannot have members.")

  final def show: String = Show(this)

  final def top: Global.Top = this match
    case top: Global.Top  => top
    case None             => throw new Exception("None does not have a top")
    case Member(owner, _) => owner.top

object Global:
  given Ordering[Global] =
    Ordering.by[Global, (String, String)] {
      case Global.Member(Global.Top(id), spec) => (id, spec.mangle)
      case Global.Top(id)                      => (id, "")
      case _                                   => ("", "")
    }
