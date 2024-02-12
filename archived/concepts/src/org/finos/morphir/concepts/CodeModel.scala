package org.finos
package morphir
package concepts

import morphir.ir.AccessControlled.AccessControlled
import morphir.ir.Documented.Documented
import morphir.ir.Module.ModuleName
import morphir.ir.Name.*
object CodeModel:
  sealed trait CodeRepr extends Product with Serializable
  sealed trait Defn     extends CodeRepr
  sealed trait Spec     extends CodeRepr

  trait DistroMethods

  enum Distro extends CodeRepr with DistroMethods:
    case Bundle()
    case Library()
    case Workspace()

  sealed trait Package extends CodeRepr
  case class PackageSpec[+TA](modules: ModuleSpecListing[TA])

  sealed trait Module       extends CodeRepr
  sealed trait ModuleMember extends CodeRepr

  type TypeMapping[Target, +TA] = Target match
    case ModuleDef[_, _] => Map[Name, AccessControlled[Documented[TypeDef[TA]]]]
    case ModuleSpec[_]   => Map[Name, AccessControlled[Documented[TypeSpec[TA]]]]

  case class ModuleDef[+TA, +VA](types: Map[Name, Any], values: Map[Name, Any]) extends Module with Defn
  case class ModuleSpec[+TA](types: Map[Name, Documented[TypeSpec[TA]]], values: Map[Name, Documented[ValueSpec[TA]]])
      extends Module
      with Spec

  opaque type ModuleSpecListing[+TA] <: Iterable[(ModuleName, ModuleSpec[TA])] = Map[ModuleName, ModuleSpec[TA]]
  object ModuleSpecListing:
    def apply[TA](entries: (ModuleName, ModuleSpec[TA])*): ModuleSpecListing[TA] = entries.toMap
    def fromMap[TA](map: Map[ModuleName, ModuleSpec[TA]]): ModuleSpecListing[TA] = map

    extension [TA](self: ModuleSpecListing[TA])
      def moduleNames: Iterable[ModuleName]      = self.keys
      def moduleSpecs: Iterable[ModuleSpec[TA]]  = self.values
      def toMap: Map[ModuleName, ModuleSpec[TA]] = self

  enum Type[+A] extends CodeRepr:
    case Unit(attributes: A)
    case Variable(attributes: A, name: Name)

  opaque type Constructors[+A] <: Map[Name, ConstructorArgs[A]] = Map[Name, ConstructorArgs[A]]
  object Constructors:
    def apply[A](ctors: Constructor[A]*): Constructors[A] = ctors.toMap

  opaque type Constructor[+A] = (Name, ConstructorArgs[A])
  object Constructor:
    def apply[A](name: Name, args: ConstructorArgs[A]): Constructor[A] = (name, args)
    extension [A](self: Constructor[A])
      def name: Name                    = self._1
      def arguments: ConstructorArgs[A] = self._2

  opaque type ConstructorArg[+A] = (Name, Type[A])
  object ConstructorArg:
    def apply[A](name: Name, argType: Type[A]): ConstructorArg[A] = (name, argType)
    extension [A](self: ConstructorArg[A])
      def name: Name       = self._1
      def argType: Type[A] = self._2

  opaque type ConstructorArgs[+A] = List[(Name, Type[A])]
  object ConstructorArgs:
    def apply[A](args: ConstructorArg[A]*): ConstructorArgs[A] = args.toList
    extension [A](self: ConstructorArgs[A])
      def names: List[Name]       = self.map(_._1)
      def argTypes: List[Type[A]] = self.map(_._2)

  sealed trait TypeSpecOrDefMethods extends ModuleMember:
    def typeParams: List[Name]
  sealed trait TypeDefMethods  extends TypeSpecOrDefMethods with Defn
  sealed trait TypeSpecMethods extends TypeSpecOrDefMethods with Spec

  enum TypeDef[+A] extends TypeDefMethods:
    case TypeAliasDef(typeParams: List[Name], typeExp: Type[A])
    case CustomTypeDef(typeParams: List[Name], ctors: AccessControlled[Constructors[A]])

  enum TypeSpec[+A] extends TypeSpecMethods:
    case TypeAliasSpec(typeParams: List[Name], typeExp: Type[A])
    case OpaqueTypeSpec(typeParams: List[Name])
    case CustomTypeSpec(typeParams: List[Name], ctors: Constructors[A])

  enum Value[+TA, +VA] extends CodeRepr:
    case Unit(attributes: VA)

  type ValueDef[+TA, +VA] = Any
  type ValueSpec[+TA]     = Any

trait CodeModelModule:
  type TypeAttribs
  type ValueAttribs

  type Type = CodeModel.Type[TypeAttribs]
  object Type:
    export CodeModel.Type.*

  type Value = CodeModel.Value[TypeAttribs, ValueAttribs]
  object Value:
    export CodeModel.Value.*
