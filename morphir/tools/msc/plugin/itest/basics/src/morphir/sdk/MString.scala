package org.finos.morphir.sdk

object MString:
  import org.finos.morphir.sdk.Bool.Bool

  opaque type MString = String

  def isEmpty(value: MString): Bool = value.isEmpty
