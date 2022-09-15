package org.finos.morphir.ir

object naming:
  opaque type Token = String
  object Token:
    def apply(str: String) = str

  opaque type Name = List[Token]
  object Name:
    def apply(first: Token, rest: Token*): Name = first :: rest.toList
    def apply(tokens: ::[Token])                = tokens

  // TODO: Work through symbol so that you get basic structural info from the type
  sealed trait TypeSym:
    def name: Name

  enum Symbol:
    case GlobalSym(packagePath: Path.PackagePath, modulePath: Path.ModulePath, name: Option[LocalSym])
    case PackageSym(path: Path.PackagePath)
    case ModuleSym(base: Path, name: LocalSym)
    case MemberSym(value: String)
    case RecordSym(name: Name)     extends Symbol with TypeSym
    case CustomTypeSym(name: Name) extends Symbol with TypeSym
    case TypeVariableSym(name: Name)
    case LocalSym(name: Name)

  sealed trait PathLike:
    def toList: List[Name]

  enum Path extends PathLike:
    case ModulePath(toList: List[Name])
    case PackagePath(toList: List[Name])

  final case class QName(modulePath: Path.ModulePath, name: Symbol.LocalSym):
    self =>
    def localName: Symbol.LocalSym       = self.name
    def toTuple: (Path, Symbol.LocalSym) = (self.modulePath, self.name)

  object QName:
    def fromTuple(tuple: (Path.ModulePath, Symbol.LocalSym)): QName = QName(tuple._1, tuple._2)

  final case class FQName(packagePath: Path.PackagePath, modulePath: Path.ModulePath, localName: Symbol.LocalSym)

  enum Global:
    case Module(packagePath: Path.PackagePath, modulePath: Path.ModulePath)
