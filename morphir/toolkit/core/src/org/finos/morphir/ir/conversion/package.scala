package org.finos.morphir
package ir

import Type.{Type, UType}
import Value.{TypedValue, Value}

package object conversion {
  final type ToMorphirUType[A] = ToMorphirType[A, scala.Unit]
  final val ToMorphirUType = ToMorphirType

  final type ToMorphirRawValue[A] = ToMorphirValue[A, scala.Unit, scala.Unit]
  final val ToMorphirRawValue = ToMorphirValue

  final type ToMorphirTypedValue[A] = ToMorphirValue[A, scala.Unit, UType]
  final val ToMorphirTypedValue = ToMorphirValue

  implicit class ToMorphirValueOps[A](val self: A) extends AnyVal {
    def toMorphirTypedValue(implicit ev: ToMorphirTypedValue[A]): TypedValue = ev(self)
  }

  implicit class ToMorphirTypeOps[A](private val self: A) extends AnyVal {
    def morphirType(implicit toMorphirUType: ToMorphirUType[A]): Type[scala.Unit] = toMorphirUType.morphirType
  }
}
