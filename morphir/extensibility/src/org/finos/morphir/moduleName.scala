package org.finos.morphir

private[morphir] trait ModuleNameExports { self: NameExports with PathExports =>

  /**
   * A module name is a unique identifier for a module within a package. It is represented by a `Path`, which is a
   * "list" of names.
   */
  sealed case class ModuleName(path: Path) { self =>

    /// Construct a new module name by concatting the given module name to this one.
    def ++(other: ModuleName): ModuleName = ModuleName(path ++ other.path)

    /// Construct a new module name by concatting the given module path to this one.
    def ++(other: Path): ModuleName = ModuleName(path ++ other)

    /// Construct a new module name by concatting the given local name to this module name.
    def /(name: Name): ModuleName = ModuleName(path / name)
    /// Construct a new module name by concatting the given local name to this module name.
    def /(name: String): ModuleName = ModuleName(path / Name(name))

    /// Check if the module name is empty.
    @inline def isEmpty: Boolean = path.isEmpty

    /// Get the name of this module.
    /// For example if the module name is `Morphir.SDK.Basics` then the name is `Basics`.
    def name: Name =
      self match {
        case ModuleName(Path(Vector())) => Name.empty
        case ModuleName(Path(segments)) => segments.last
      }

    // Get the name of this module if a name is present.
    def nameOption: Option[Name] =
      self match {
        case ModuleName(Path(Vector())) => None
        case ModuleName(Path(segments)) => Some(segments.last)
      }

    /// Convert this module name to a `Path`.
    @inline def toPath: Path      = path
    override def toString: String = path.toString
  }

  object ModuleName {
    /// Create an empty module name.
    val empty: ModuleName = ModuleName(Path.empty)

    def apply(input: String): ModuleName =
      ModuleName.fromPath(Path.fromArray(input.split('.').map(Name.fromString)))

    def fromPath(path: Path): ModuleName      = ModuleName(path)
    def fromString(input: String): ModuleName = ModuleName(input)
  }

}
