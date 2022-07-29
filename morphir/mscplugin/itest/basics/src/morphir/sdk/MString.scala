package morphir.sdk

object MString:
  import morphir.sdk.Bool.Bool

  opaque type MString = String

  def isEmpty(value: MString): Bool = value.isEmpty
