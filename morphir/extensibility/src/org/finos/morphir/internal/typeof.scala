package org.finos.morphir.internal
import org.finos.morphir.naming._
import org.finos.morphir.extensibility.SdkModuleDescriptors._

trait TypeOfModule { self: TypeModule with TypeInfoModule =>

  trait TypeOf[T] {
    final def tpe: Type[_] = typeInfo.tpe
    def typeInfo: TypeInfo[_]
  }

  object TypeOf {
    lazy val morphirSdkPackageName: PackageName = PackageName.fromString("Morphir.SDK")

    def apply[T](implicit typeOf: TypeOf[T]): TypeOf[T] = typeOf

    implicit val boolean: TypeOf[Boolean] = new TypeOf[Boolean] {
      override def typeInfo: TypeInfo[_] = {
        val fqn = Morphir.SDK.Basics.fqn("Bool")
        val tpe = Type.Reference((), fqn, List.empty)
        TypeInfo.TypeOnly(tpe)
      }
    }
  }

}
