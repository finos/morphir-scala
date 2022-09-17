package org.finos
package morphir
package mir.types.recursive

import org.finos.morphir.mir.{FQName, Name}
//import org.finos.morphir.sdk.ResultModule.Result

trait TypeModule extends AllTypeSyntax {
  final type Type[+A] = morphir.mir.types.recursive.Type[A]
  val Type: morphir.mir.types.recursive.Type.type = morphir.mir.types.recursive.Type

  final type Constructors[+A] = morphir.mir.types.recursive.Constructors[A]
  val Constructors: morphir.mir.types.recursive.Constructors.type = morphir.mir.types.recursive.Constructors

  final type Definition[+A] = morphir.mir.types.recursive.Definition[A]
  val Definition: morphir.mir.types.recursive.Definition.type = morphir.mir.types.recursive.Definition

  final type Field[+A] = morphir.mir.types.recursive.Field[A]
  val Field: morphir.mir.types.recursive.Field.type = morphir.mir.types.recursive.Field

  final type Specification[+A] = morphir.mir.types.recursive.Specification[A]
  val Specification: morphir.mir.types.recursive.Specification.type = morphir.mir.types.recursive.Specification

  final type UConstructors = morphir.mir.types.recursive.Constructors[Any]
  val UConstructors: morphir.mir.types.recursive.Constructors.type = morphir.mir.types.recursive.Constructors

  final type UDefinition = morphir.mir.types.recursive.Definition[Any]
  val UDefinition: morphir.mir.types.recursive.Definition.type = morphir.mir.types.recursive.Definition

  final type UType = morphir.mir.types.recursive.UType
  val UType: morphir.mir.types.recursive.UType.type = morphir.mir.types.recursive.UType

  final def definitionToSpecification[A](definition: Definition[A]): Specification[A] = definition.toSpecification

  final def mapSpecificationAttributes[A, B](f: A => B)(spec: Specification[A]): Specification[B] = spec.map(f)
  final def mapSpecificationAttributes[A](spec: Specification[A]): Specification.MapSpecificationAttributes[A] =
    Specification.mapSpecificationAttributes(spec)

//    spec.map(f)

//  def mapDefinition[A, B, E](
//      f: Type[A] => Result[E, Type[B]],
//      definition: Definition[A]
//  ): Result[List[E], Definition[B]] =
//    definition.map(f)

  def mapDefinitionAttributes[A, B](f: A => B, defn: Definition[A]): Definition[B] = ??? // defn.map(f)

  final def mapTypeAttributes[A, B](f: A => B, tpe: Type[A]): Type[B]     = tpe.mapAttributes(f)
  final def mapTypeAttributes[A](tpe: Type[A]): Type.MapTypeAttributes[A] = Type.mapTypeAttributes(tpe)

  def typeAttributes[A](tpe: Type[A]): A = tpe.attributes

  def eraseAttributes[A](typeDef: Definition[A]): UDefinition = typeDef.eraseAttributes

  def collectVariables[A](tpe: Type[A]): Set[Name]    = tpe.collectVariables
  def collectReferences[A](tpe: Type[A]): Set[FQName] = tpe.collectReferences
  def substituteTypeVariables[A](mapping: Map[Name, Type[A]], tpe: Type[A]): Type[A] =
    ??? // tpe.substituteTypeVariables

  def toString[A](tpe: Type[A]): String = tpe.toString
}

object TypeModule extends TypeModule
