package morphir

// TODO:  Reintroduce later
// import morphir.sdk.ResultModule

package object ir {

  // TODO:  Reintroduce later
  // type LiteralValue = Literal[Any]
  // val LiteralValue: Literal.type = Literal

  type ModulePath = Module.ModulePath
  val ModulePath: Module.ModulePath.type = Module.ModulePath

  type PackageName = PackageModule.PackageName
  val PackageName: PackageModule.PackageName.type = PackageModule.PackageName

  // TODO:  Reintroduce later
  // type PackageSpecification[+Annotations] = PackageModule.Specification[Annotations]
  // val PackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  // type Result[+E, +A] = zio.morphir.sdk.ResultModule.Result[E, A]
  // val Result: ResultModule.Result.type = zio.morphir.sdk.ResultModule.Result

  // type UPackageSpecification = PackageModule.Specification[Any]
  // val UPackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type ??? = Nothing

  // TODO:  Reintroduce later
  // final implicit class StringOps(private val self: String) extends AnyVal {
  //   import zio.morphir.ir.types.nonrecursive
  //   import zio.morphir.ir.types.recursive

  //   def <:>[A](tpe: recursive.Type[A]): recursive.Field[recursive.Type[A]] =
  //     recursive.Field(Name.fromString(self), tpe)

  //   def <:>[A](tpe: nonrecursive.Type[A]): nonrecursive.Field[nonrecursive.Type[A]] =
  //     nonrecursive.Field(Name.fromString(self), tpe)

  //   def :=(value: Int): Value.Value.LetDefinition.Unbound[Any, Type.UType] =
  //     Value.Value.LetDefinition.Unbound(Name.fromString(self), Value.Definition.fromLiteral(Literal.int(value)))

  // }
}
