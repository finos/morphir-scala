package org.finos.morphir
package ir
package internal

import zio.Chunk

trait MorphirValueModule extends TypeModuleExported with PatternModule with LiteralModule { module =>
  type Value[+TA, +VA]
  final type RawValue       = Value[scala.Unit, scala.Unit]
  final type TypedValue     = Value[scala.Unit, UType]
  final type USpecification = Specification[scala.Unit]

  sealed case class Definition[+TA, +VA](
      inputTypes: Chunk[(Name, VA, Type[TA])],
      outputType: Type[TA],
      body: Value[TA, VA]
  ) { self =>
    def toSpecification: Specification[TA] =
      Specification(self.inputTypes.map { case (name, _, tpe) => name -> tpe }, self.outputType)

  }

  object Definition {
    type Raw   = Definition[scala.Unit, scala.Unit]
    type Typed = Definition[scala.Unit, UType]
  }

  sealed case class Specification[+TA](inputs: Chunk[(Name, Type[TA])], output: Type[TA]) { self =>
    def map[B](f: TA => B): Specification[B] =
      Specification(
        inputs = inputs.map { case (name, tpe) => (name, Type.mapTypeAttributes(tpe)(f)) },
        output = Type.mapTypeAttributes(output)(f)
      )
  }

  object Specification {
    def create[Attributes](inputs: (Name, Type[Attributes])*): Inputs[Attributes] =
      new Inputs(() => Chunk.fromIterable(inputs))

    type Raw = Specification[scala.Unit]
    object Raw {
      def apply(inputs: (String, UType)*)(output: UType): Raw =
        Specification(
          inputs = Chunk.fromIterable(inputs.map { case (n, t) => Name.fromString(n) -> t }),
          output = output
        )
    }

    final class Inputs[A](private val inputs: () => Chunk[(Name, Type[A])]) {
      def apply(output: Type[A]): Specification[A] =
        Specification(inputs(), output)
    }
  }

  final def definitionToSpecification[TA, VA](definition: Definition[TA, VA]): Specification[TA] =
    definition.toSpecification

  def reference[VA](attributes: VA, fullyQualifiedName: FQName)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA]
  final def reference(fullyQualifiedName: FQName): Value[Nothing, scala.Unit] = reference((), fullyQualifiedName)

  def unit[VA](attributes: VA)(implicit ev: NeedsAttributes[VA]): Value[Nothing, VA]
  final val unit: Value[Nothing, scala.Unit] = unit(())

  def update[TA, VA](attributes: VA, valueToUpdate: Value[TA, VA], fieldsToUpdate: Map[Name, Value[TA, VA]])(implicit
      ev: NeedsAttributes[VA]
  ): Value[TA, VA] = ???
}
