package org.finos.morphir
package runtime

import org.finos.morphir.naming._
trait Runtime { self =>
  import Runtime._

  type TypeAttribs
  type ValueAttribs

  final type Environment = EvaluationEnvironment[TypeAttribs, ValueAttribs]
  final val Environment: EvaluationEnvironment.type = EvaluationEnvironment
  final def makeEnvironment(): Environment          = Environment()

  abstract class StatefulTreeWalker[State, +A](initialState: State) {
    private var state: State = initialState
    def getState: State      = state
    def visitUnit(attributes: ValueAttribs): A
    def visitVariable(attributes: ValueAttribs, name: Name): A
  }

}

object Runtime {
  final case class EvaluationEnvironment[+TA, +VA]()
}
