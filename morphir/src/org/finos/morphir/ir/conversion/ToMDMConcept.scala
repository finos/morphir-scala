package org.finos.morphir
package ir
package conversion

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.{Concept, Label}
import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.Type.{Type, UType}

import scala.collection.immutable.{Map, Set}
import org.finos.morphir.runtime.Distributions

trait ToMDMConcept[A] { self =>
  import ToMDMConcept.*
  def apply: Concept
  final def concept(distributions: Distributions, bindings: Map[Name, Concept] = Map.empty): ToMDMConceptEither = apply

  def as[B]: ToMDMConcept[B] = new ToMDMConcept[B] {
    override def apply(distributions: Distributions, bindings: Map[Name, Concept] = Map.empty): Concept =
      self.apply(distributions, bindings)
  }
}

object ToMDMConcept {

  sealed trait NoEquivalentMDM extends Throwable
  type ToMDMConceptEither = Either[NoEquivalentMDM, Concept]

  def apply[A](implicit toMDMConcept: ToMDMConcept[A]): ToMDMConcept[A] = toMDMConcept
  def summon[A]: SummonPartiallyApplied[A]                              = new SummonPartiallyApplied[A]

  def toUTypeConverter[A](f: => UType): ToMDMConcept[A] = new ToMDMConcept[A] {
    def apply: UType = f
  }

  implicit def uTypeToConcept(tpe: UType): ToMDMConcept[UType] =
    concept match {
      case Concept.Boolean                 => boolUType.as
      case Concept.Byte                    => byteUType.as
      case Concept.Float                   => decimalUType.as
      case Concept.Decimal                 => decimalUType.as
      case Concept.Integer                 => bigIntUType.as
      case Concept.Int16                   => shortUType.as
      case Concept.Int32                   => intUType.as
      case Concept.Int64                   => intUType.as
      case Concept.String                  => stringUType.as
      case Concept.LocalDate               => localDateUType.as
      case Concept.Month                   => monthUType.as
      case Concept.DayOfWeek               => dayOfWeekUType.as
      case Concept.LocalTime               => localTimeUType.as
      case Concept.Char                    => charUType.as
      case Concept.Order                   => toUTypeConverter(sdk.Basics.orderType)
      case Concept.Unit                    => unitUType.as
      case Concept.Alias(name, _)          => toUTypeConverter(T.reference(name))
      case Concept.Enum(name, _)           => toUTypeConverter(T.reference(name))
      case Concept.List(elementType)       => listUType(conceptToTypeIR(elementType)).as
      case Concept.Optional(elementType)   => optionUType(conceptToTypeIR(elementType)).as
      case Concept.Result(errType, okType) => resultUType(conceptToTypeIR(errType), conceptToTypeIR(okType)).as
      case Concept.Map(keyType, valueType) => mapUType(conceptToTypeIR(keyType), conceptToTypeIR(valueType)).as
      case Concept.Set(elementType)        => setUType(conceptToTypeIR(elementType)).as
      case Concept.Struct(fields) =>
        val types: scala.List[(String, UType)] = fields.map {
          case (k: Label, v: Concept) => (k.value, conceptToTypeIR(v).concept)
        }
        toUTypeConverter(T.record(types: _*))

      // Treat a record as an aliased reference on the type level, on the value level it has fields
      // Record('Pack.Mod.Person', [name, age]) on the type-level is just Reference(Pack.Mod.Person)
      // on the value level it's the equivalent of a record behind a type alias e.g. `person:Person; person = {name:"", age:""}`
      case Concept.Record(name, _) =>
        toUTypeConverter(T.reference(name))

      case Concept.Tuple(values) =>
        val types: scala.List[UType] = values.map(conceptToTypeIR(_).concept)
        toUTypeConverter(T.tupleVar(types: _*))

      case Concept.Any      => toUTypeConverter(sdk.Basics.neverType) // TODO: map this to the correct type
      case Concept.Nothing  => toUTypeConverter(sdk.Basics.neverType) // TODO: map this to the correct type
      case Concept.Union(_) => toUTypeConverter(sdk.Basics.neverType) // TODO: map this to the correct type
    }

  final class SummonPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def withAttributesOf[Attribs](implicit toMDMConcept: ToMDMConcept[A, Attribs]): ToMDMConcept[A, Attribs] =
      toMDMConcept
  }
}
