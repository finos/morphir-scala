package org.finos.morphir.internal
import org.finos.morphir.naming.*
import org.finos.morphir.extensibility.SdkModuleDescriptors.*
import org.finos.morphir.util.attribs.Attributes

trait TypeOfModule { self: TypeModule with TypeInfoModule =>

  trait TypeOf[T] {
    def apply(): TypeInfo[Attributes]
    final def tpe: Type[Attributes]          = typeInfo.tpe
    final def typeInfo: TypeInfo[Attributes] = apply()
  }

  object TypeOf {
    lazy val morphirSdkPackageName: PackageName = PackageName.fromString("Morphir.SDK")

    def apply[T](implicit typeOf: TypeOf[T]): TypeOf[T] = typeOf

    implicit val boolean: TypeOf[Boolean] = () => {
      val fqn = Morphir.SDK.Basics.fqn("Bool")
      val tpe = Type.Reference(Attributes.empty, fqn, List.empty)
      TypeInfo.fromType(tpe)
    }
  }


}
