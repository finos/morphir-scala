package org.finos.morphir

private[morphir] trait FQNameExports {
  self: NameExports with ModuleNameExports with PackageNameExports with PathExports with QualifiedModuleNameExports
    with QNameExports =>

  sealed case class FQName(packagePath: PackageName, modulePath: ModuleName, localName: Name) {
    def getPackagePath: Path = packagePath.toPath

    def getModulePath: Path = modulePath.toPath

    def toReferenceName: String = Seq(
      Path.toString(Name.toTitleCase, ".", packagePath.toPath),
      Path.toString(Name.toTitleCase, ".", modulePath.toPath),
      localName.toTitleCase
    ).mkString(".")

    override def toString: String = Array(
      Path.toString(Name.toTitleCase, ".", packagePath.toPath),
      Path.toString(Name.toTitleCase, ".", modulePath.toPath),
      Name.toCamelCase(localName)
    ).mkString(":")
  }

  object FQName {
    //    def apply(packagePath: Path, modulePath: Path, localName: Name): FQName =
    //      FQName(PackageName(packagePath), ModulePath(modulePath), localName)

    val fqName: Path => Path => Name => FQName = packagePath =>
      modulePath => localName => FQName(PackageName.fromPath(packagePath), ModuleName.fromPath(modulePath), localName)

    def fromQName(packagePath: Path, qName: QName): FQName =
      FQName(PackageName.fromPath(packagePath), ModuleName(qName.modulePath), qName.localName)

    def fromQName(qName: QName)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, ModuleName.fromPath(QName.getModulePath(qName)), QName.getLocalName(qName))

    /** Get the package path part of a fully-qualified name. */
    def getPackagePath(fqName: FQName): Path = fqName.getPackagePath

    /** Get the module path part of a fully-qualified name */
    def getModulePath(fqName: FQName): Path = fqName.getModulePath

    /** Get the local name part of a fully-qualified name */
    def getLocalName(fqName: FQName): Name = fqName.localName

    /** Convenience function to create a fully-qualified name from 3 strings */
    def fqn(packageName: String, moduleName: String, localName: String): FQName =
      FQName(PackageName.fromString(packageName), ModuleName.fromString(moduleName), Name.fromString(localName))

    /** Convenience function to create a fully-qualified name from 2 strings with default package name */
    def fqn(moduleName: String, localName: String)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, ModuleName.fromPath(Path.fromString(moduleName)), Name.fromString(localName))

    /** Convenience function to create a fully-qualified name from 1 string with defaults for package and module */
    def fqn(localName: String)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, options.defaultModule, Name.fromString(localName))

    /// Convenience function to create a fully-qualified name from a local name and an implicitly provided `QualifiedModuleName`.
    def fromLocalName(localName: String)(implicit qualifiedModuleName: QualifiedModuleName): FQName =
      FQName(qualifiedModuleName.packageName, qualifiedModuleName.modulePath, Name.fromString(localName))

    def toString(fqName: FQName): String = fqName.toString

    /** Parse a string into a FQName using splitter as the separator between package, module, and local names */
    def fromString(fqNameString: String, splitter: String)(implicit options: FQNamingOptions): FQName =
      fqNameString.split(splitter) match {
        case Array(packageNameString, moduleNameString, localNameString) =>
          fqn(packageNameString, moduleNameString, localNameString)
        case Array(moduleNameString, localNameString) =>
          fqn(moduleNameString, localNameString)
        case Array(localNameString) =>
          fqn(localNameString)
        case _ => throw FQNameParsingError(fqNameString)
      }

    def fromString(fqNameString: String)(implicit options: FQNamingOptions): FQName =
      fromString(fqNameString, options.defaultSeparator)
  }

  sealed case class FQNamingOptions(defaultPackage: PackageName, defaultModule: ModuleName, defaultSeparator: String)

  object FQNamingOptions {
    implicit val default: FQNamingOptions =
      FQNamingOptions(PackageName.empty, ModuleName.empty, ":")
  }

  sealed case class FQNameParsingError(invalidName: String)
      extends Exception(s"Unable to parse: [$invalidName] into a valid FQName")
}
