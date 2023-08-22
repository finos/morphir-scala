package org.finos.morphir.datamodel

import org.finos.morphir.naming._

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

  implicit class QualifiedNameHelper(val sc: StringContext) extends AnyVal {
    def qn(args: Any*): FQName = {
      val interlaced = interlace(sc.parts, args.map(_.toString)).mkString
      // Allow deliniation via ":" or "::"
      // Make sure :: is 1st here otherwise regex won't split right
      val splits = interlaced.split("::|:")
      if (splits.length != 3)
        throw new IllegalArgumentException(
          "Fully qualified name must follow the convension package/name::module/name::localname"
        )
      val packageSplits = splits(0)
      val moduleSplits  = splits(1)
      val localName     = splits(2)

      val packageName = {
        val segments = packageSplits.split("/").toList.map(s => Name(s))
        PackageName.fromIterable(segments)
      }
      val namespace = {
        val segments = moduleSplits.split("/").toList.map(s => Name(s))
        ModuleName.fromIterable(segments)
      }

      FQName(packageName, namespace, Name.fromString(localName))
    }
  }

  def interlace[T](a: Iterable[T], b: Iterable[T]): List[T] =
    if (a.isEmpty) b.toList
    else a.head +: interlace(b, a.tail)
}
