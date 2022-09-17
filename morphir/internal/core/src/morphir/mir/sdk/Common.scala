package org.finos.morphir.mir.sdk

import zio.Chunk
import org.finos.morphir.mir.Module.ModuleName
import org.finos.morphir.mir.PackageModule.PackageName
import org.finos.morphir.mir.Type.UType
import org.finos.morphir.mir.{Value, _}
object Common {
  val packageName: PackageName = PackageName.fromString("Morphir.SDK")

  def toFQName(moduleName: ModuleName, localName: String): FQName =
    FQName(packageName, moduleName.toModulePath, Name.fromString(localName))

  def tFun(firstArgType: UType, rest: UType*)(returnType: UType): UType = tFun(firstArgType :: rest.toList, returnType)
  def tFun(argTypes: List[UType], returnType: UType): UType             = UType.curriedFunction(argTypes, returnType)

  def tVar(varName: String): UType = UType.variable(varName)

  def vSpec(name: String, inputs: (String, UType)*) = new VSpec(() => (name, Chunk.fromIterable(inputs)))

  final class VSpec(private val data: () => (String, Chunk[(String, UType)])) extends AnyVal {
    def apply(outputType: UType): (Name, Documented[Value.Specification[Any]]) = {
      val (name, inputs) = data()
      (
        Name.fromString(name),
        Documented(
          "documentation",
          Value.Specification(inputs.map { case (name, tpe) => (Name.fromString(name), tpe) }, outputType)
        )
      )
    }

    @inline def returning(outputType: UType): (Name, Documented[Value.Specification[Any]]) = apply(outputType)
  }

}
