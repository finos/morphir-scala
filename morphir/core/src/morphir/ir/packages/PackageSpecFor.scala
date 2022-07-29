package morphir.mir.packages

import morphir.mir.{FQName, NativeFunction}

trait PackageSpecFor[A] {
  import PackageModule._

  def packageName: PackageName
  def spec: Specification[Any]
  def nativeFunctions: Map[FQName, NativeFunction]
}

object PackageSpecFor {
  def apply[A](implicit packageSpecFor: PackageSpecFor[A]): PackageSpecFor[A] = packageSpecFor
}
