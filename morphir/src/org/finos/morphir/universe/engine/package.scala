package org.finos.morphir.universe
import zio.prelude.*
import org.finos.morphir.core.capabilities.free.Free

package object engine {
  type Instruction[+E, +A] = Free[Instr, E, A]
}
