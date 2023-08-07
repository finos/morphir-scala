package org.finos.morphir

private[morphir] trait QNameExports { self: NameExports with PathExports =>

  /// A qualified name (`QName`) is a combination of a module path and a local name.
  sealed case class QName(modulePath: Path, localName: Name) {

    /// Turn a qualified name into a tuple of a module path and a local name.
    @inline def toTuple: (Path, Name) = (modulePath, localName)

    override def toString: String =
      modulePath.toString(Name.toTitleCase, ".") + ":" + localName.toCamelCase

  }

  object QName {
    /// Turn a qualified name into a tuple of a module path and a local name.
    def toTuple(qName: QName): (Path, Name) = qName.toTuple

    /// Turn a tuple of a module path and a local name into a qualified name (`QName`).
    def fromTuple(tuple: (Path, Name)): QName = QName(tuple._1, tuple._2)

    /// Creates a qualified name from a module path and a local name.
    def fromName(modulePath: Path, localName: Name): QName = QName(modulePath, localName)

    /// Creates a qualified name from strings representing a module path and a local name.
    def fromName(modulePath: String, localName: String): QName =
      QName(Path.fromString(modulePath), Name.fromString(localName))

      /// Get the local name part of a qualified name.
    def getLocalName(qname: QName): Name = qname.localName

    /// Get the module path part of a qualified name.
    def getModulePath(qname: QName): Path = qname.modulePath

    /// Turn a `QName` into a string using `:` as the separator between the module path and the local name.
    def toString(qName: QName): String = qName.toString

    /// Parse a string into a qualified name using `:` as the separator between the module path and the local name.
    def fromString(str: String): Option[QName] =
      str.split(":") match {
        case Array(packageNameString, localNameString) =>
          Some(QName(Path.fromString(packageNameString), Name.fromString(localNameString)))
        case _ => None
      }
  }
}
