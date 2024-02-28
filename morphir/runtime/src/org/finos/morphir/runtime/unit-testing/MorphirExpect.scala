package org.finos.morphir.runtime
import org.finos.morphir.naming.*

sealed trait MorphirExpect{
  def arity : Int;
  def localName : Local
}

