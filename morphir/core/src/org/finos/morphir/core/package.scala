package org.finos.morphir

import org.finos.morphir.core.types.Naming.Name

package object core:
  export org.finos.morphir.core.internal.Visitor

  def name(input: String): Name = Name(input)
