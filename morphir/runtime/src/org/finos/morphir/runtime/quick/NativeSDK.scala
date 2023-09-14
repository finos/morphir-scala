package org.finos.morphir.runtime
package quick

import org.finos.morphir.{Hints, MorphirTag, naming}
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.quick.nativesdk._

// Needs to be in the same file as implementors (I.e. Basic, List, etc...) so that this can be a sealed
// abstract class (which we can then see the instances of with macros etc...).
sealed abstract class SdkModuleDescriptor(moduleName: String)(implicit packageName: PackageName)
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
}

object NativeSDK {
  import Coercer._

  implicit val packageName: PackageName = PackageName.fromString("Morphir.SDK")

  case object Basics extends SdkModuleDescriptor("Basics") {
    val functions = scala.List(
      NativeFunctionAdapter.Fun2(BasicsSDK.modBy.asNative2),
      NativeFunctionAdapter.Fun2(BasicsSDK.greaterThan.asNative2),
      NativeFunctionAdapter.Fun2(BasicsSDK.greaterThanOrEqual.asNative2),
      NativeFunctionAdapter.Fun2(BasicsSDK.lessThan.asNative2),
      NativeFunctionAdapter.Fun2(BasicsSDK.lessThanOrEqual.asNative2)
    )
  }

  case object List extends SdkModuleDescriptor("List") {
    val functions = scala.List(
      NativeFunctionAdapter.Fun1(ListSDK.concat),
      NativeFunctionAdapter.Fun1(ListSDK.singleton),
      NativeFunctionAdapter.Fun1(ListSDK.isEmpty),
      NativeFunctionAdapter.Fun1(ListSDK.length),
      NativeFunctionAdapter.Fun2(ListSDK.filter),
      NativeFunctionAdapter.Fun2(ListSDK.map),
      NativeFunctionAdapter.Fun2(ListSDK.any),
      NativeFunctionAdapter.Fun2(ListSDK.partition),
      NativeFunctionAdapter.Fun3(ListSDK.foldl),
      NativeFunctionAdapter.Fun2(ListSDK.append),
      NativeFunctionAdapter.Fun2(ListSDK.cons)
    )
  }

  val resolvedFunctions: Map[FQName, SDKValue] =
    Basics.resolvedFunctions ++ List.resolvedFunctions
}
