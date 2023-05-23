package org.finos.morphir

// TODO:  Reintroduce later
// import org.finos.morphir.sdk.ResultModule

package object ir {

  type ModuleName = Module.ModuleName
  val ModuleName: Module.ModuleName.type = Module.ModuleName

  type PackageName = PackageModule.PackageName
  val PackageName: PackageModule.PackageName.type = PackageModule.PackageName

  // TODO:  Reintroduce later
  // type PackageSpecification[+Annotations] = PackageModule.Specification[Annotations]
  // val PackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  // type Result[+E, +A] = morphir.sdk.ResultModule.Result[E, A]
  // val Result: ResultModule.Result.type = morphir.sdk.ResultModule.Result

  // type UPackageSpecification = PackageModule.Specification[Any]
  // val UPackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type ??? = Nothing

  // TODO:  Reintroduce later
  final implicit class StringOps(private val self: String) extends AnyVal {
    import org.finos.morphir.ir.Type.{FieldT, Type}

    def <:>[A](tpe: Type[A]): FieldT[A] =
      Field(Name.fromString(self), tpe)

    //   def <:>[A](tpe: nonrecursive.Type[A]): nonrecursive.Field[nonrecursive.Type[A]] =
    //     nonrecursive.Field(Name.fromString(self), tpe)

    //   def :=(value: Int): Value.Value.LetDefinition.Unbound[Any, Type.UType] =
    //     Value.Value.LetDefinition.Unbound(Name.fromString(self), Value.Definition.fromLiteral(Literal.int(value)))

  }

  final implicit class StringContextOps(private val self: StringContext) extends AnyVal {
    def n(args: Any*): Name =
      self.parts.toList match {
        case List(name) => Name.fromString(name)
        case _          => throw new IllegalArgumentException("Name must be a single string")
      }
  }
}
