package org.finos.morphir.universe

import org.finos.morphir.mir

import zio.=!=
import zio.prelude._

package object ir {

  type AccessControlled[+A] = mir.AccessControlled[A]
  val AccessControlled: mir.AccessControlled.type = mir.AccessControlled

  type Documented[+A] = mir.Documented[A]
  val Documented: mir.Documented.type = mir.Documented

  type UType = RawType
  val UType: RawType.type = RawType

  type RawType = RawType.Type
  object RawType extends Subtype[Type[scala.Unit]]

  type RawTypeInfo = RawTypeInfo.Type
  object RawTypeInfo extends Subtype[TypeInfo[scala.Unit]] {
    def apply[A](typeInfo: TypeInfo[A])(implicit ev: A =!= scala.Unit): RawTypeInfo = wrap(typeInfo.map(_ => ()))

  }
}
