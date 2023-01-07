package org.finos.morphir

package object core:
  export org.finos.morphir.core.internal.InternalTypes.*
  export org.finos.morphir.core.internal.Visitor

  def name(input: CharSequence): Name = Name(input)
