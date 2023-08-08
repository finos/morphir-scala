package org.finos.morphir.universe
import zio.=!=
import zio.prelude.*

package object ir {

  type UType = RawType
  val UType: RawType.type = RawType

  type RawType = RawType.Type
  object RawType extends Subtype[Type[scala.Unit]]

  type RawTypeInfo = RawTypeInfo.Type
  object RawTypeInfo extends Subtype[TypeInfo[scala.Unit]] {
    def apply[A](typeInfo: TypeInfo[A])(implicit ev: A =!= scala.Unit): RawTypeInfo = wrap(typeInfo.map(_ => ()))

  }
}
