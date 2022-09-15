package org.finos.morphir.ir

object naming:
  opaque type Token = String
  object Token:
    def apply(str: String) = str

  opaque type Name = List[Token]
  object Name:
    def apply(first: Token, rest: Token*): Name = first :: rest.toList
    def apply(tokens: ::[Token])                = tokens

  sealed trait TypeName
  enum Marker:
    case FQN(packagePath: Path.PackagePath, modulePath: Path.ModulePath, name: LocalName)
    case PackageName(path: Path.PackagePath)
    case ModuleName(base: Path, name: LocalName)
    case MemberName(value: String)
    case RecordName(value: String)
    case TypeVariableName(value: String)
    case LocalName(value: String)

  sealed trait PathLike:
    def toList: List[Name]

  enum Path extends PathLike:
    case ModulePath(toList: List[Name])
    case PackagePath(toList: List[Name])

  final case class QName(modulePath: Path.ModulePath, name: Marker.LocalName):
    self =>
    def localName: Marker.LocalName       = self.name
    def toTuple: (Path, Marker.LocalName) = (self.modulePath, self.name)

  object QName:
    def fromTuple(tuple: (Path.ModulePath, Marker.LocalName)): QName = QName(tuple._1, tuple._2)

  final case class FQName(packagePath: Path.PackagePath, modulePath: Path.ModulePath, localName: Marker.LocalName)

  enum Global:
    case Module(packagePath: Path.PackagePath, modulePath: Path.ModulePath)
