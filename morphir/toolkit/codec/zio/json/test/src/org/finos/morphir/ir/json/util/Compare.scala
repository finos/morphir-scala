package org.finos.morphir.ir.json.util

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.collection.immutable

object Compare {
  sealed trait Diff
  object Diff {
    case class MissingRight(leftValue: Any) extends Diff
    case class MissingLeft(rightValue: Any) extends Diff

    case class Object(typename: String, fields: ListMap[String, Diff])   extends Diff
    case class Sequence(typename: String, fields: ListMap[String, Diff]) extends Diff

    case class Leaf(typename: String, a: Any, b: Any)                      extends Diff
    case class Leaf2(typenameA: String, typenameB: String, a: Any, b: Any) extends Diff

    case class Set(typename: String, onlyLeft: immutable.Set[Any], onlyRight: immutable.Set[Any]) extends Diff

    implicit class WithFieldExt(diff: Option[Diff]) {
      def withField(field: String) =
        diff.map(v => (field, v))
    }

    object Leaf {
      def of[T](a: T, b: T)(implicit ct: ClassTag[T]): Option[Diff] =
        if (a == b)
          None
        else
          Some(Leaf(className(ct.runtimeClass), a, b))
      def ofClass(aCls: Class[_], bCls: Class[_])(a: Any, b: Any): Option[Diff] =
        if (aCls != bCls && a != b)
          Some(Leaf2(className(aCls), className(bCls), a, b))
        else if (aCls == bCls && a != b)
          Some(Leaf(className(aCls), a, b))
        else
          None
    }
  }

  def apply(a: Any, b: Any): Option[Diff] = generic(a, b)
  private def generic(aValue: Any, bValue: Any): Option[Diff] =
    (aValue, bValue) match {
      case _ if (aValue == bValue) => None
      // Needs specialized by-content comparator for sets
      case (a: Set[_], b: Set[_])           => set(a, b)
      case (a: Iterable[_], b: Iterable[_]) => iterable(a, b)
      case (a: Int, b: Int)                 => Diff.Leaf.of(a, b)
      case (a: Short, b: Short)             => Diff.Leaf.of(a, b)
      case (a: Boolean, b: Boolean)         => Diff.Leaf.of(a, b)
      case (a: Double, b: Double)           => Diff.Leaf.of(a, b)
      case (a: Float, b: Float)             => Diff.Leaf.of(a, b)
      case (a: Byte, b: Byte)               => Diff.Leaf.of(a, b)
      case (a: Char, b: Char)               => Diff.Leaf.of(a, b)
      case (a: Product, b: Product)         => product(a, b)
      case (a: AnyRef, b: AnyRef)           => Diff.Leaf.ofClass(a.getClass, b.getClass)(a, b)
    }

  private def set(ai: Set[_], bi: Set[_]): Option[Diff] = {
    val a     = ai.asInstanceOf[Set[Any]]
    val b     = bi.asInstanceOf[Set[Any]]
    val onlyA = a.removedAll(b)
    val onlyB = b.removedAll(a)
    if (onlyA.isEmpty && onlyB.isEmpty)
      None
    else
      Some(Diff.Set("Set", onlyA, onlyB))
  }

  private def iterable(ai: Iterable[_], bi: Iterable[_]): Option[Diff] = {
    val fields =
      ai.map(Option(_))
        .zipAll(bi.map(Option(_)), None, None)
        .zipWithIndex
        .map {
          // TODO Compare(0, a, b, c) to Compare(a, b, c) should return diff 0
          case ((Some(a), Some(b)), index) => generic(a, b).withField(index.toString)
          case ((None, Some(a)), index)    => Some(Diff.MissingLeft(a)).withField(index.toString)
          case ((Some(a), None), index)    => Some(Diff.MissingRight(a)).withField(index.toString)
          case _                           => None
        }
        .collect { case Some(v) => v }
        .toList
    if (fields.isEmpty)
      None
    else
      Some(Diff.Sequence(className(ai.getClass), ListMap.from(fields)))
  }

  private def trySimpleName(cls: Class[_]) =
    try
      cls.getSimpleName
    catch {
      case e: Throwable => cls.getName
    }

  private def className(cls: Class[_]) = {
    val ancestry  = new Ancestry(cls)
    val matchSeq  = new MatchAncestry("scala.collection.Seq")
    val matchList = new MatchAncestry("scala.collection.List")
    ancestry match {
      case matchSeq(listName)  => listName
      case matchList(listName) => listName
      case other               => trySimpleName(other.cls)
    }
  }

  private def product(a: Product, b: Product): Option[Diff] =
    if (a.getClass != b.getClass) Diff.Leaf.ofClass(a.getClass, b.getClass)(a, b)
    else {
      val clsName = className(a.getClass)
      val names   = a.productElementNames.toList
      val as      = a.productIterator.toList
      val bs      = b.productIterator.toList
      if (names.length != as.length || as.length != bs.length)
        throw new RuntimeException(
          s"""Different element lengths found:
             |========== Names =========
             |${names.zipWithIndex.map { case (v, i) => s"$i - $v" }.mkString("\n")}
             |========== AS ========
             |${as.zipWithIndex.map { case (v, i) => s"$i - $v" }.mkString("\n")}
             |========== BS ========
             |${bs.zipWithIndex.map { case (v, i) => s"$i - $v" }.mkString("\n")}""".stripMargin
        )
      if (as == bs) None
      else {
        val fields =
          ListMap.from(
            names
              .zip(as)
              .zip(bs)
              .map { case ((name, a), b) => generic(a, b).withField(name) }
              .collect { case Some((field, value)) => (field, value) }
          )
        Some(Diff.Object(clsName, fields))
      }
    }

  class Ancestry(val cls: Class[_]) {
    def ancestry(cls: Class[_]): List[Class[_]] = {
      val superCls = cls.getSuperclass
      if (superCls != null) cls +: ancestry(superCls)
      else cls :: Nil
    }
    val interfaces = ancestry(cls).flatMap(cls => cls.getInterfaces)
  }
  class MatchAncestry(name: String) {
    def unapply(ancestry: Ancestry): Option[String] =
      ancestry.interfaces.find(_.getName.matches(name)).map(trySimpleName(_))
  }
}
