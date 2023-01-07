package org.finos.morphir

package object core:
  export org.finos.morphir.core.internal.InternalTypes.*

  def name(input: CharSequence): Name = Name(input)
