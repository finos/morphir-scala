package org.finos.morphir
package ir

import Type.Type
import zio.Tag

trait ToMorphirType[A, +Attribs] {
  def apply(implicit tag: Tag[A]): Type[Attribs]
}

object ToMorphirType {}

// Given some type in Scala... to represent thart type in Morphir
