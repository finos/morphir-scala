package org.finos.morphir
package ir
package sdk

import zio.Chunk
import org.finos.morphir.ir.Module.QualifiedModuleName
import org.finos.morphir.ir.PackageModule.PackageName
import org.finos.morphir.ir.Type._
object Common {
  val packageName: PackageName = PackageName.fromString("Morphir.SDK")

  def toFQName(moduleName: QualifiedModuleName, localName: String): FQName =
    FQName(packageName, moduleName.toModulePath, Name.fromString(localName))

  def tFun(firstArgType: UType, rest: UType*)(returnType: UType): UType = tFun(firstArgType :: rest.toList, returnType)
  def tFun(argTypes: List[UType], returnType: UType): UType             = curriedFunction(argTypes, returnType)

  def tVar(varName: String): UType = variable(varName)

  def vSpec(name: String, inputs: (String, UType)*) = new VSpec(() => (name, Chunk.fromIterable(inputs)))

  final class VSpec(private val data: () => (String, Chunk[(String, UType)])) extends AnyVal {
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
