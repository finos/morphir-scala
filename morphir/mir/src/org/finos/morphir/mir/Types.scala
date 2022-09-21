package org.finos
package morphir
package mir
import util.{unreachable, unsupported}

sealed abstract class Type:
  def show: String   = morphir.mir.Show(this)
  def mangle: String = morphir.mir.Mangle(this)

object Type:
  /** Value types are either primitive or aggregate. */
  sealed abstract class ValueKind extends Type

  /** Primitive value types. */
  sealed abstract class PrimitiveKind(val width: Int) extends ValueKind
  case object Bool                                    extends PrimitiveKind(1)

  sealed abstract class RefKind                                                        extends Type
  case object Null                                                                     extends RefKind
  case object Unit                                                                     extends RefKind
  final case class Array(ty: Type, nullable: Boolean = true)                           extends RefKind
  final case class Ref(name: Global, exact: Boolean = false, nullable: Boolean = true) extends RefKind
