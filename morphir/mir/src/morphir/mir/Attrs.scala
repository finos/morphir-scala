package morphir.mir

enum Attr:
  case Extern
  case Opaque

  def show: String = Show(this)

final case class Attrs(isExtern: Boolean = false, isOpaque: Boolean = false):
  def show: String = Show(this)

  def toSeq: Seq[Attr] =
    val out = Seq.newBuilder[Attr]
    if (isExtern) out += Attr.Extern
    if (isOpaque) out += Attr.Opaque
    out.result()

object Attrs:
  val None = new Attrs()

  def fromSeq(attrs: Seq[Attr]): Attrs =
    var isExtern = false
    var isOpaque = false

    attrs.foreach {
      case Attr.Extern => isExtern = true
      case Attr.Opaque => isOpaque = true
    }
    Attrs(isExtern = isExtern)
