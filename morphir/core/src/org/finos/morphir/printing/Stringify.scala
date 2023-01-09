package org.finos.morphir.printing

trait Stringify[A]:
  def apply(value: A): String

  extension (value: A)(using renderer: Stringify[A])
    def stringify: String     = renderer(value)
    def stringifyLine: String = renderer(value) + System.lineSeparator()

object Stringify extends StringifyLow0

def apply[A](using printer: Stringify[A]): Stringify[A] = printer

private[printing] trait StringifyLow0:
  given [A]: Stringify[A] = value => value.toString
