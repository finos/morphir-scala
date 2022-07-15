package morphir.knowledge.logic
import zio.stream.ZStream
package object core {
  type FieldConstraint = PartialFunction[State, State]
  type Name            = String
  type Value           = Any
  type SStream         = Flux[State]
  val SStream = Flux
}
