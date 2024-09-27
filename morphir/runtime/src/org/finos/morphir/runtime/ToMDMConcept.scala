package org.finos.morphir.runtime

import org.finos.morphir.ir.conversion

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.{Concept, Label}
import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.Type.{Type, UType}

import scala.collection.immutable.{Map, Set}
import org.finos.morphir.runtime.Distributions

trait ToMDMConcept[A] { self =>
  import ToMDMConcept.*
  def apply(distributions: Distributions, bindings: Map[Name, Concept] = Map.empty): ToMDMConceptEither
  final def concept(distributions: Distributions, bindings: Map[Name, Concept] = Map.empty): ToMDMConceptEither =
    apply(distributions, bindings)

  def as[B]: ToMDMConcept[B] = new ToMDMConcept[B] {
    override def apply(distributions: Distributions, bindings: Map[Name, Concept] = Map.empty): ToMDMConceptEither =
      self.apply(distributions, bindings)
  }
}

object ToMDMConcept {

  sealed trait NoEquivalentMDM extends Throwable
  type ToMDMConceptEither = Either[NoEquivalentMDM, Concept]

  def apply[A](implicit toMDMConcept: ToMDMConcept[A]): ToMDMConcept[A] = toMDMConcept
  // def summon[A]: SummonPartiallyApplied[A]                              = new SummonPartiallyApplied[A]

  def toConceptConverter[A](f: => Concept): ToMDMConcept[A] = new ToMDMConcept[A] {
    def apply(distributions: Distributions, bindings: Map[Name, Concept] = Map.empty): ToMDMConceptEither = Right(f)
  }

  implicit val unitUType: ToMDMConcept[scala.Unit] = toConceptConverter(Concept.Unit)

  implicit def uTypeToConcept(tpe: UType): ToMDMConcept[UType] =
    tpe match {
      case _ => toConceptConverter(Concept.Unit)
    }

  // final class SummonPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
  //   def withAttributesOf[Attribs](implicit toMDMConcept: ToMDMConcept[A, Attribs]): ToMDMConcept[A, Attribs] =
  //     toMDMConcept
  // }
}
