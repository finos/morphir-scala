package org.finos.morphir.datamodel

import org.finos.morphir.naming.*
import zio.Chunk

class QualifiedNameCollector extends ConceptStatefulTransformer[Chunk[FQName]] {
  private def addToState(v: Concept)(value: FQName) =
    Stateful.succeedWithState(v)(chunk => chunk :+ value)

  override def of(c: Concept) =
    c match {
      case v @ Concept.Record(name, _) => addToState(v)(name)
      case v @ Concept.Alias(name, _)  => addToState(v)(name)
      case v @ Concept.Enum(name, _)   => addToState(v)(name)
      case v: Concept.List             => addToState(v)(FQName.fromString("Morphir.SDK:List:List"))
      case v: Concept.Map              => addToState(v)(FQName.fromString("Morphir.SDK:Dict:Dict"))
      case v: Concept.Set              => addToState(v)(FQName.fromString("Morphir.SDK:Set:Set"))
      case v: Concept.Basic[_]         =>
        v match {
          // Assuming that ToMorphirValue maps bytes to ints and this is a "standard" definition
          case Concept.Byte      => throw new RuntimeException("Morphir Byte (Int8) not supported yet")
          case Concept.Decimal   => addToState(v)(FQName.fromString("Morphir.SDK:Decimal:Decimal"))
          case Concept.Integer   => throw new RuntimeException("Morphir Integer (BigInt) not supported yet")
          case Concept.Int16     => addToState(v)(FQName.fromString("Morphir.SDK:Int:Int16"))
          case Concept.Int32     => addToState(v)(FQName.fromString("Morphir.SDK:Int:Int32"))
          case Concept.Int64     => addToState(v)(FQName.fromString("Morphir.SDK:Int:Int64"))
          case Concept.LocalDate => addToState(v)(FQName.fromString("Morphir.SDK:LocalDate:LocalDate"))
          case Concept.Month     => addToState(v)(FQName.fromString("Morphir.SDK:Month:Month"))
          case Concept.LocalTime => addToState(v)(FQName.fromString("Morphir.SDK:LocalTime:LocalTime"))
          case other             => super.of(other)
        }

      case other => super.of(other)
    }
}
object QualifiedNameCollector {
  def collectFrom(c: Concept) =
    (new QualifiedNameCollector().of(c)).run(Chunk[FQName]()) match {
      case (chunk, _) => chunk
    }
}
