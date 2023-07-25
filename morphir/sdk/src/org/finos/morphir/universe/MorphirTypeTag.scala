package org.finos.morphir.universe

import org.finos.morphir.universe.ir.RawTypeInfo
import izumi.reflect.Tag

trait MorphirTypeTag[A] {
  type Underyling = A

  def typeInfo(tag: Tag[A]): MorphirType
}

object MorphirTypeTag {
  def apply[A](implicit tag: MorphirTypeTag[A]): MorphirTypeTag[A] = tag

  // def succeed[A](f: => RawTypeInfo): MorphirTypeTag[A] = new MorphirTypeTag[A] {
  //   override def typeInfo: RawTypeInfo = f
  // }

}
