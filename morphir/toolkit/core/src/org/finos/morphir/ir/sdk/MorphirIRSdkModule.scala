package org.finos.morphir.ir.sdk

import org.finos.morphir.naming._
import org.finos.morphir.ir.{Documented, Value}
import org.finos.morphir.ir.Type._
import zio.Chunk
abstract class MorphirIRSdkModule(modulePathString: String) {
  implicit val packageName: PackageName = PackageName.fromString("Morphir.SDK")
  val moduleName: ModuleName            = ModuleName.fromString(modulePathString)
  final lazy val qualifiedModuleName: QualifiedModuleName =
    QualifiedModuleName(packageName, moduleName)

  def name(name: String): Name                          = Name.fromString(name)
  def pkg(name: String): PackageName                    = PackageName.fromString(name)
  final def moduleName(name: String): ModuleName        = ModuleName.fromString(name)
  final def fqn(localName: String): FQName              = FQName(packageName, moduleName, Name.fromString(localName))
  @inline final def toFQName(localName: String): FQName = fqn(localName)

  def tFun(firstArgType: UType, rest: UType*)(returnType: UType): UType = tFun(firstArgType :: rest.toList, returnType)
  def tFun(argTypes: List[UType], returnType: UType): UType             = curriedFunction(argTypes, returnType)

  def tVar(varName: String): UType = variable(varName)

  def vSpec(name: String, inputs: (String, UType)*) = new VSpec(() => (name, Chunk.fromIterable(inputs)))

  final class VSpec(private val data: () => (String, Chunk[(String, UType)])) {
    def apply(outputType: UType): (Name, Documented[Value.USpecification]) = {
      val (name, inputs) = data()
      (
        Name.fromString(name),
        Documented(
          "documentation",
          Value.Specification(inputs.map { case (name, tpe) => (Name.fromString(name), tpe) }, outputType)
        )
      )
    }

    @inline def returning(outputType: UType): (Name, Documented[Value.USpecification]) = apply(outputType)
  }
}
