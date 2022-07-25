package morphir.ir.packages

final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) {
  self =>
  def %(name: Name): FQName = FQName(packageName, modulePath, name)

  def %(name: String): FQName = FQName(packageName, modulePath, Name.fromString(name))
}
