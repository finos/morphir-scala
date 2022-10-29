package org.finos.morphir
package ir
package internal

import Literal.Lit
import Type.{Type, UType}
import Value.{RawValue, TypedValue}
import zio.Chunk

private[internal] final case class ValueDefinition[+TA, +VA](
    inputTypes: Chunk[(Name, VA, Type[TA])],
    outputType: Type[TA],
    body: Value[TA, VA]
) { self =>
  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): ValueDefinition[TB, VB] = ???
  def toSpecification: ValueSpecification[TA] =
    ValueSpecification(self.inputTypes.map { case (name, _, tpe) => name -> tpe }, self.outputType)

}

object ValueDefinition {
  def apply[TA, VA](outputType: Type[TA], body: Value[TA, VA]): ValueDefinition[TA, VA] =
    ValueDefinition(Chunk.empty, outputType, body)

  def apply[TA, VA](
      inputTypes: (String, VA, Type[TA])*
  )(outputType: Type[TA])(body: Value[TA, VA]): ValueDefinition[TA, VA] = {
    val args = Chunk.fromIterable(inputTypes.map { case (n, va, t) => (Name.fromString(n), va, t) })
    ValueDefinition(args, outputType, body)
  }

  def fromLiteral(literal: Lit): Typed = {
    val tpe = literal.inferredType
    ValueDefinition(outputType = tpe, body = Value.Literal(tpe, literal))
  }

  def fromRawValue(value: (RawValue, UType)): ValueDefinition.Raw =
    ValueDefinition(
      inputTypes = Chunk.empty,
      outputType = value._2,
      body = value._1
    )

  def fromRawValue(value: RawValue, outputType: UType): ValueDefinition.Raw =
    ValueDefinition(
      inputTypes = Chunk.empty,
      outputType = outputType,
      body = value
    )

  def fromTypedValue(value: TypedValue): ValueDefinition.Typed =
    ValueDefinition(
      inputTypes = Chunk.empty,
      outputType = value.attributes,
      body = value
    )

  type Raw = ValueDefinition[scala.Unit, scala.Unit]
  object Raw {

    def apply(inputTypes: Chunk[(Name, UType)], outputType: UType, body: RawValue): Raw =
      ValueDefinition(inputTypes.map { case (n, t) => (n, (), t) }, outputType, body)

    def apply(outputType: UType, body: RawValue): Raw =
      ValueDefinition(
        inputTypes = Chunk.empty,
        outputType = outputType,
        body = body
      )

    def apply(inputTypes: (String, UType)*)(outputType: UType)(body: RawValue): Raw = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), (), t) })
      ValueDefinition(args, outputType, body)
    }
  }
  type Typed = ValueDefinition[scala.Unit, UType]
  object Typed {
    def apply(inputTypes: (String, UType)*)(outputType: UType)(body: TypedValue): Typed = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), t, t) })
      ValueDefinition(args, outputType, body)
    }
  }
}
