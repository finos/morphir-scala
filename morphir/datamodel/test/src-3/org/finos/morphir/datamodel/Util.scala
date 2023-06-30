package org.finos.morphir.datamodel

object Util {
  implicit class LabelHelper(val sc: StringContext) extends AnyVal {
    def l(args: Any*): Label = {
      val interlaced = interlace(sc.parts, args.map(_.toString))
      Label(interlaced.mkString)
    }
  }

  implicit class EnumLabelHelper(val sc: StringContext) extends AnyVal {
    def el(args: Any*): EnumLabel = {
      val interlaced = interlace(sc.parts, args.map(_.toString))
      EnumLabel(interlaced.mkString)
    }
  }

  def interlace[T](a: Iterable[T], b: Iterable[T]): List[T] =
    if (a.isEmpty) b.toList
    else a.head +: interlace(b, a.tail)
}
