package org.finos.morphir

import org.finos.morphir.core.types.Naming.Name

package object core {
  type Visitor[TA, VA, -In, +Out] = internal.Visitor[TA, VA, In, Out]
  val Visitor = internal.Visitor

  def name(input: String): Name = Name(input)
}
