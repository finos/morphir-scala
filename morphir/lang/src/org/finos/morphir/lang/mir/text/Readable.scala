package org.finos.morphir.lang.mir.text
import upickle.core.Visitor
trait Readable:
  def transform[T](f: Visitor[_, T]): T
