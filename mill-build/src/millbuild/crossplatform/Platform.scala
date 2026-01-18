package millbuild.crossplatform

import upickle.default.{ReadWriter => RW, readwriter}

sealed trait Platform extends Ordered[Platform] { self =>
  val isJS: Boolean        = self == Platform.JS
  val isJVM: Boolean       = self == Platform.JVM
  val isNative: Boolean    = self == Platform.Native
  val isNotNative: Boolean = !isNative
  def name: String
  def compare(that: Platform): Int = self.name.compare(that.name)

  def suffixes: Seq[String] = Seq(self.name) ++ pairs.map {
    case (a, b) => a + "-" + b
  }
  final def pairs: Seq[(String, String)] =
    Platform.all.filter(_ != self).map { p =>
      if (self < p) (self.name, p.name)
      else (p.name, self.name)
    }.toSeq

  override def toString = name
}
object Platform {
  lazy val all: Set[Platform] = Set(JVM, JS, Native)

  private def fromName(name: String): Platform = name match {
    case "jvm"    => JVM
    case "js"     => JS
    case "native" => Native
    case other    => throw new IllegalArgumentException(s"Unknown platform: $other")
  }

  implicit val rw: RW[Platform] = readwriter[String].bimap(_.name, fromName)

  type JVM = Platform.JVM.type
  case object JVM extends Platform {
    implicit val rw: RW[JVM.type] = readwriter[String].bimap(_.name, _ => JVM)
    def name                      = "jvm"
  }
  type JS = Platform.JS.type
  case object JS extends Platform {
    implicit val rw: RW[JS.type] = readwriter[String].bimap(_.name, _ => JS)
    def name                     = "js"
  }
  type Native = Platform.Native.type
  case object Native extends Platform {
    implicit val rw: RW[Native.type] = readwriter[String].bimap(_.name, _ => Native)
    def name                         = "native"
  }

  sealed trait FolderMode { self =>
    import FolderMode._

    def modeName: String = self match {
      case UseNesting => "nesting"
      case UseSuffix  => "suffix"
      case UseBoth    => "both"
    }

    final def useNesting: Boolean =
      self match {
        case UseBoth    => true
        case UseNesting => true
        case _          => false
      }

    final def useSuffix: Boolean =
      self match {
        case UseNesting => false
        case UseSuffix  => true
        case UseBoth    => true
      }

  }
  object FolderMode {
    private def fromModeName(name: String): FolderMode = name match {
      case "nesting" => UseNesting
      case "suffix"  => UseSuffix
      case "both"    => UseBoth
      case other     => throw new IllegalArgumentException(s"Unknown folder mode: $other")
    }

    implicit val rw: RW[FolderMode] = readwriter[String].bimap(_.modeName, fromModeName)

    type UseNesting = UseNesting.type
    case object UseNesting extends FolderMode {
      implicit val rw: RW[UseNesting.type] = readwriter[String].bimap(_.modeName, _ => UseNesting)
    }

    type UseSuffix = UseSuffix.type
    case object UseSuffix extends FolderMode {
      implicit val rw: RW[UseSuffix.type] = readwriter[String].bimap(_.modeName, _ => UseSuffix)
    }

    type UseBoth = UseBoth.type
    case object UseBoth extends FolderMode {
      implicit val rw: RW[UseBoth.type] = readwriter[String].bimap(_.modeName, _ => UseBoth)
    }
  }

}
