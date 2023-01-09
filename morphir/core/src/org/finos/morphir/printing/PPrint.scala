package org.finos.morphir.printing

trait PPrint[T]:
  def render(value: T): Option[pprint.Tree]

object PPrint

trait PPrintLow0:
  given [T]: PPrint[T] = _ => None
