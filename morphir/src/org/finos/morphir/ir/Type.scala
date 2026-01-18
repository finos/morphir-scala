package org.finos.morphir
package ir

import org.finos.morphir.naming._
import io.TypeWriter
import scala.annotation.{tailrec, unused}
import zio._
import zio.prelude._

object Type extends TypeModule

trait TypeModule { module =>

  final type Field[+A] = universe.ir.Field[A]
  final val Field: universe.ir.Field.type = universe.ir.Field

  final type FieldK[F[+_], +A] = universe.ir.FieldK[F, A]
  final val FieldK: universe.ir.Field.type = universe.ir.Field

  final type FieldT[+A] = universe.ir.FieldT[A]
  final val FieldT: universe.ir.FieldT.type = universe.ir.FieldT

  final type Type[+A] = org.finos.morphir.universe.ir.Type[A]
  final val Type = org.finos.morphir.universe.ir.Type

  final type UConstructors = module.Constructors[scala.Unit]
  final val UConstructors: module.Constructors.type = module.Constructors

  final type UDefinition = module.Definition[scala.Unit]
  final val UDefinition: module.Definition.type = module.Definition

  final type USpecification = module.Specification[scala.Unit]

  final type UType = module.Type[scala.Unit]
  final val UType = module.Type

  final def curriedFunction(paramTypes: List[UType], returnType: UType): UType = {
    def curry(args: List[UType]): UType = args match {
      case Nil                    => returnType
      case firstArg :: restOfArgs => function(firstArg, curry(restOfArgs))
    }
    curry(paramTypes)
  }

  final def defineField(name: Name, fieldType: UType): FieldT[Unit] = FieldT(name, fieldType)

  final def defineField(name: String, fieldType: UType): FieldT[Unit] = FieldT(Name.fromString(name), fieldType)

  final def emptyTuple[A](attributes: A)(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    Type.Tuple(attributes, List.empty)
  lazy val emptyTuple: UType = Type.Tuple((), List.empty)

  // Extensible record constructors
  def extensibleRecord[A](attributes: A, name: Name, fields: List[FieldT[A]])(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    Type.ExtensibleRecord(attributes, name, fields)

  final def extensibleRecord[A](attributes: A, name: String, fields: List[FieldT[A]])(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), fields)

  final def extensibleRecord[A](attributes: A, name: String, field: FieldT[A], fields: FieldT[A]*)(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), field :: fields.toList)

  final def extensibleRecord[A](attributes: A, name: Name, fields: (String, Type[A])*)(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] = {
    val fieldsChunk = fields.map { case (name, typeExpr) =>
      FieldT(Name.fromString(name), typeExpr)
    }.toList
    Type.ExtensibleRecord(attributes, name, fieldsChunk)
  }

  final def extensibleRecord[A](attributes: A, name: String, fields: (String, Type[A])*)(implicit
      ev: NeedsAttributes[A]
  ): Type[A] =
    extensibleRecord(attributes, Name.fromString(name), fields: _*)

  final def extensibleRecord(name: Name, fields: List[FieldT[Unit]]): UType =
    Type.ExtensibleRecord((), name, fields)

  final def extensibleRecord(name: String, fields: List[FieldT[Unit]]): UType =
    Type.ExtensibleRecord((), Name.fromString(name), fields)

  final def extensibleRecord(name: Name, fields: (String, UType)*): UType = {
    val fieldsList = fields.map { case (name, typeExpr) => FieldT(Name.fromString(name), typeExpr) }.toList
    Type.ExtensibleRecord((), name, fieldsList)
  }

  final def extensibleRecord(name: String, fields: (String, UType)*): UType =
    extensibleRecord(Name.fromString(name), fields: _*)

  final def extensibleRecordWithFields(name: Name, fields: FieldT[Unit]*): UType =
    Type.ExtensibleRecord((), name, fields.toList)

  final def extensibleRecordWithFields(name: String, fields: FieldT[Unit]*): UType =
    Type.ExtensibleRecord((), Name.fromString(name), fields.toList)

  final def field[A](name: String, tpe: Type[A]): FieldT[A] = FieldT(Name.fromString(name), tpe)
  final def field[A](name: Name, tpe: Type[A]): FieldT[A]   = FieldT(name, tpe)

  final def field[A](tuple: (String, Type[A])): FieldT[A] = FieldT(Name.fromString(tuple._1), tuple._2)

  final def fields[A](fields: Field[A]*): List[Field[A]] = fields.toList

  // Function constructors
  final def function[A](attributes: A, argumentType: Type[A], returnType: Type[A])(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    Type.Function(attributes, argumentType, returnType)

  final def function(argumentType: UType, returnType: UType): UType =
    Type.Function((), argumentType, returnType)

  final def mapTypeAttributes[A, B](tpe: Type[A])(f: A => B): Type[B] = tpe.mapAttributes(f)

  // Record constructors
  final def record[A](attributes: A, fields: List[FieldT[A]])(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    Type.Record(attributes, fields)

  final def record(fields: List[FieldT[Unit]]): UType =
    Type.Record((), fields)

  final def record(field: FieldT[Unit], fields: FieldT[Unit]*): UType =
    Type.Record((), field :: fields.toList)

  final def record(fields: (String, UType)*): UType =
    Type.Record((), fields.map { case (name, typeExpr) => FieldT(Name.fromString(name), typeExpr) }.toList)

  final def reference[A](attributes: A, typeName: FQName, typeParams: List[Type[A]])(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    Type.Reference(attributes, typeName, typeParams)

  final def reference[A](attributes: A, typeName: FQName)(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    Type.Reference(attributes, typeName, List.empty)

  final def reference[A](attributes: A, typeName: FQName, firstTypeParam: Type[A], otherTypeParams: Type[A]*)(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    Type.Reference(attributes, typeName, firstTypeParam :: otherTypeParams.toList)

  final def reference[A](attributes: A, typeName: String, typeParams: List[Type[A]])(implicit
      @unused ev: NeedsAttributes[A]
  ) =
    Type.Reference(attributes, FQName.fromString(typeName), typeParams: _*)

  final def reference[A](attributes: A, typeName: String)(implicit @unused ev: NeedsAttributes[A]) =
    Type.Reference(attributes, FQName.fromString(typeName), List.empty)

  final def reference[A](attributes: A, typeName: String, firstTypeParam: Type[A], otherTypeParams: Type[A]*)(implicit
      @unused ev: NeedsAttributes[A]
  ) =
    Type.Reference(attributes, FQName.fromString(typeName), firstTypeParam :: otherTypeParams.toList)

  final def reference(typeName: FQName, typeParams: List[UType]): UType =
    Type.Reference((), typeName, typeParams.toList)

  final def reference(typeName: FQName, typeParams: UType*): UType =
    Type.Reference((), typeName, typeParams.toList)

  final def reference(typeName: String, typeParams: List[UType]): UType =
    Type.Reference((), FQName.fromString(typeName), typeParams)

  final def reference(typeName: String, typeParams: UType*): UType =
    Type.Reference((), FQName.fromString(typeName), typeParams.toList)

  final def reference(packageName: String, moduleName: String, typeName: String, typeParams: UType*): UType =
    Type.Reference((), FQName.fqn(packageName, moduleName, typeName), typeParams.toList)

  final def reference(packageName: String, moduleName: String, typeName: String, typeParams: List[UType]): UType =
    Type.Reference((), FQName.fqn(packageName, moduleName, typeName), typeParams)

  // Tuple constructors
  final def tupleWithAttr[A](attributes: A, elements: List[Type[A]])(implicit
      @unused ev: NeedsAttributes[A]
  ): Type[A] =
    Type.Tuple(attributes, elements.toList)

  final def tupleWithAttr[A](attributes: A, elements: Type[A]*)(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    tupleWithAttr(attributes, elements.toList)

  final def tupleVar(elements: UType*): UType =
    Type.Tuple((), elements.toList)

  final def tuple(elements: List[UType]): UType =
    Type.Tuple((), elements.toList)

  final def typeAttributes[A](tpe: Type[A]): A = tpe.attributes

  final def unit[A](attributes: A)(implicit @unused ev: NeedsAttributes[A]): Type[A] = Type.Unit(attributes)

  final lazy val unit: UType     = Type.Unit(())
  final lazy val unitType: UType = Type.Unit(())

  // Variable constructors
  final def variable[A](attributes: A, name: Name)(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    Type.Variable(attributes, name)

  final def variable[A](attributes: A, name: String)(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    Type.Variable(attributes, Name.fromString(name))

  final def variable(name: Name): UType =
    Type.Variable((), name)

  final def variable(name: String): UType =
    Type.Variable((), Name.fromString(name))

  sealed case class Constructors[+Attribs](toMap: Map[Name, Chunk[(Name, Type[Attribs])]]) {
    self =>
    def eraseAttributes: UConstructors = Constructors(toMap.map { case (ctor, args) =>
      (ctor, args.map { case (paramName, paramType) => (paramName, paramType.eraseAttributes) })
    })

    def collectReferences: Set[FQName] =
      toMap.values.foldLeft(Set.empty[FQName]) { case (acc, ctors) =>
        ctors.foldLeft(acc) { case (acc, (_, paramType)) => acc ++ paramType.collectReferences }
      }

    def ctorNames: Set[Name] = toMap.keySet

    def map[B](f: Attribs => B): Constructors[B] =
      Constructors(toMap.map { case (name, ctors) => (name, ctors.map { case (name, tpe) => (name, tpe.map(f)) }) })
  }

  object Constructors {
    def forEnum(case1: String, otherCases: String*): UConstructors = {
      val allCases  = (Chunk(case1) ++ otherCases).map(Name.fromString)
      val emptyArgs = Chunk[(Name, UType)]()
      Constructors(allCases.map(name => (name, emptyArgs)).toMap)
    }

  }

  sealed trait Definition[+Attribs] { self =>
    import Definition._
    import Specification._

    def collectReferences: Set[FQName] = self match {
      case TypeAlias(_, typeExp)                     => typeExp.collectReferences
      case CustomType(_, AccessControlled(_, value)) => value.collectReferences
    }

    def eraseAttributes: UDefinition = self match {
      case TypeAlias(typeParams, typeExp) =>
        TypeAlias(typeParams, typeExp.eraseAttributes)
      case CustomType(typeParams, ctors) =>
        CustomType(typeParams, ctors.map(_.eraseAttributes))
    }

    def map[B](f: Attribs => B): Definition[B] = self match {
      case TypeAlias(typeParams, typeExp) => TypeAlias(typeParams, typeExp.map(f))
      case CustomType(typeParams, ctors)  => CustomType(typeParams, ctors.map(_.map(f)))
    }

    def toSpecification: Specification[Attribs] = self match {
      case TypeAlias(typeParams, typeExp) => TypeAliasSpecification(typeParams, typeExp)
      case CustomType(typeParams: Chunk[Name], AccessControlled.WithPublicAccess(ctors)) =>
        CustomTypeSpecification(typeParams, ctors)
      case CustomType(typeParams, _) =>
        OpaqueTypeSpecification(typeParams)
    }
  }

  object Definition {

    sealed case class TypeAlias[+Attribs](typeParams: Chunk[Name], typeExp: Type[Attribs])
        extends Definition[Attribs]

    sealed case class CustomType[+Attribs](
        typeParams: Chunk[Name],
        ctors: AccessControlled[Constructors[Attribs]]
    ) extends Definition[Attribs]

    sealed implicit class DefinitionExtensions[A](private val self: Definition[A]) {
      // def mapAttributes[B](f:A => B)
    }
  }

  sealed trait Specification[+Attribs] { self =>
    import Specification._

    def ??(doc: String): Documented[Specification[Attribs]] =
      Documented(doc, self)

    def map[B](f: Attribs => B): Specification[B] = self match {
      case TypeAliasSpecification(typeParams, expr)   => TypeAliasSpecification(typeParams, expr.map(f))
      case spec @ OpaqueTypeSpecification(_)          => spec
      case CustomTypeSpecification(typeParams, ctors) => CustomTypeSpecification(typeParams, ctors.map(f))
    }

    def eraseAttributes: Specification[scala.Unit] = self match {
      case c @ TypeAliasSpecification(_, _) =>
        TypeAliasSpecification(c.typeParams, c.expr.eraseAttributes)
      case c @ OpaqueTypeSpecification(_) =>
        OpaqueTypeSpecification(c.typeParams)
      case c @ CustomTypeSpecification(_, _) =>
        CustomTypeSpecification(c.typeParams, c.ctors.eraseAttributes)
    }
  }

  object Specification {
    def mapSpecificationAttributes[A](spec: Specification[A]): MapSpecificationAttributes[A] =
      new MapSpecificationAttributes(() => spec)

    sealed case class TypeAliasSpecification[+Attribs](
        typeParams: Chunk[Name],
        expr: Type[Attribs]
    ) extends Specification[Attribs]

    sealed case class OpaqueTypeSpecification(typeParams: Chunk[Name]) extends Specification[Nothing]

    object OpaqueTypeSpecification {
      def apply(typeParams: String*): OpaqueTypeSpecification =
        OpaqueTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)))
    }

    sealed case class CustomTypeSpecification[+Attribs](
        typeParams: Chunk[Name],
        ctors: Constructors[Attribs]
    ) extends Specification[Attribs]

    object CustomTypeSpecification {
      def fromCtors[Attribs](
          ctor: (String, Iterable[(String, Type[Attribs])]),
          ctors: (String, Iterable[(String, Type[Attribs])])*
      ): CustomTypeSpecification[Attribs] = {
        val allCtors = (ctor +: ctors).map { case (name, args) =>
          (
            Name.fromString(name),
            Chunk.fromIterable(args.map { case (name, tpe) =>
              (Name.fromString(name), tpe)
            })
          )
        }.toMap
        CustomTypeSpecification(Chunk.empty, Constructors(allCtors))
      }

      def mkEnum(case1: String, otherCases: String*): CustomTypeSpecification[scala.Unit] =
        CustomTypeSpecification(Chunk.empty, Constructors.forEnum(case1, otherCases: _*))
    }

    // type UCustomTypeSpecification = CustomTypeSpecification[scala.Unit]
    // val UCustomTypeSpecification: CustomTypeSpecification.type = CustomTypeSpecification

    // type USpecification = module.Specification[scala.Unit]
    // val USpecification: Specification.type = module.Specification

    sealed class MapSpecificationAttributes[+A](val input: () => Specification[A]) {
      def map[B](f: A => B): Specification[B] = input().map(f)
    }

    def fromDefinition[A](definition: Definition[A]): Specification[A] = definition match {
      case Definition.TypeAlias(typeParams, typeExp)     => TypeAliasSpecification(typeParams, typeExp)
      case Definition.CustomType(typeParams, acessCtors) =>
        acessCtors.withPublicAccess match {
          case Some(ctors) => CustomTypeSpecification(typeParams, ctors)
          case None        => OpaqueTypeSpecification(typeParams)
        }
    }
  }

  trait MorphirTypeModule {
    def typeAttributes[A](tpe: Type[A]): A                        = module.typeAttributes(tpe)
    def mapTypeAttributes[A, B](tpe: Type[A])(f: A => B): Type[B] = module.mapTypeAttributes(tpe)(f)
  }

}
