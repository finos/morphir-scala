package org.finos.morphir
package runtime

import org.finos.morphir.ir.Type.UType
import org.finos.morphir.naming._
trait Runtime { self =>
  import Runtime._

  final type Environment = EvaluationEnvironment[scala.Unit, UType]
  final val Environment: EvaluationEnvironment.type = EvaluationEnvironment
  final def makeEnvironment(): Environment          = Environment()

  abstract class StatefulTreeWalker[State, +A](initialState: State) {
    private var state: State = initialState

    def getState: State = state
    def setState(newState: State): Unit =
      state = newState
    def visitUnit(attributes: UType): A
    def visitVariable(attributes: UType, name: Name): A
  }

}

object Runtime {
  final case class EvaluationEnvironment[+TA, +VA]()
}
