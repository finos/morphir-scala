package org.finos
package morphir
package ir

import prelude.*
import Name.Name
import Type.Type

object Value extends ValueVersionSpecific {

  final case class Specification[+TA](inputs: Chunk[SpecParameter[TA]], output: Type[TA]) { self =>
    def map[TB](f: TA => TB): Specification[TB] =
      Specification(inputs.map(_.map(f)), output.map(f))
  }
  sealed trait Value[+TA, +VA]
  case class Unit[+VA](attributes: VA)                 extends Value[Nothing, VA]
  case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
}
