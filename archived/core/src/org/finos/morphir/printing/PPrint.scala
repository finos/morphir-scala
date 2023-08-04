package org.finos.morphir.printing

trait PPrint[T] {
  def render(value: T): Option[pprint.Tree]
}

object PPrint

trait PPrintLow0 {
  implicit def noPPrint[T]: PPrint[T] = _ => None
}
