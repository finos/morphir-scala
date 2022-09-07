package morphir.mir.serialization

/** Serialization tags are unique type ids used to identify types in the binary
 *  representation of NIR. There are some holes in the numbering of the types to
 *  allow for binary-compatible leeway with adding new IR nodes.
 */
object Tags:
  // Attributes
  final val Attr = 0

  final val Global = Attr + 32
  final val NoneGlobal = 1 + Global
  final val TopGlobal = 1 + NoneGlobal
  final val MemberGlobal = 1 + TopGlobal
