package org.finos.morphir.extensibility
import org.finos.morphir.MorphirTag
import org.finos.morphir.naming.*
import org.finos.morphir.*
import org.finos.morphir.runtime.internal.*
import org.finos.morphir.runtime.{SDKConstructor, SDKValue}

abstract class SdkModuleDescriptor(moduleName: String)(implicit packageName: PackageName)
    extends ModuleDescriptor {
  override implicit val qualifiedModuleName: QualifiedModuleName =
    QualifiedModuleName(packageName, ModuleName.fromString(moduleName))

  def fqn(name: String): FQName = FQName(
    qualifiedModuleName.packageName,
    qualifiedModuleName.modulePath,
    Name.fromString(name)
  )

  def functions: List[NativeFunctionAdapter]

  lazy val resolvedFunctions: Map[FQName, SDKValue] =
    functions.map(adapter => (fqn(adapter.dnf.name)) -> adapter.realize).toMap

  def ctors: Map[FQName, SDKConstructor] = Map.empty
}
