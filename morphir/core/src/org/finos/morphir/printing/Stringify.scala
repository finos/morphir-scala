  package org.finos.morphir.printing

trait Stringify[A] {
  def apply(value: A): String
}

object Stringify extends StringifyLow0 {
  def apply[A](implicit printer: Stringify[A]): Stringify[A] = printer

  final implicit class StringifyOps[A](val value: A) extends AnyVal {
    def stringify(implicit renderer: Stringify[A]): String     = renderer(value)
    def stringifyLine(implicit renderer: Stringify[A]): String = renderer(value) + System.lineSeparator()
  }
}

private[printing] trait StringifyLow0 {
  implicit def defaultStringify[A]: Stringify[A] = value => value.toString
}
