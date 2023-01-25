package org.finos.morphir.ir.extras
import enumeratum.*

sealed trait NodeTag
sealed trait ValueTag extends EnumEntry with NodeTag
object ValueTag {
  case object Apply         extends ValueTag
  case object Constructor   extends ValueTag
  case object Destructure   extends ValueTag
  case object Field         extends ValueTag
  case object FieldFunction extends ValueTag
  case object IfThenElse    extends ValueTag
  case object Lambda        extends ValueTag
  case object LetDefinition extends ValueTag
  case object LetRecursion  extends ValueTag
  case object List          extends ValueTag
  case object Literal       extends ValueTag
  case object PatternMatch  extends ValueTag
  case object Record        extends ValueTag
  case object Reference     extends ValueTag
  case object Tuple         extends ValueTag
  case object Unit          extends ValueTag
  case object UpdateRecord  extends ValueTag
}

sealed trait TypeTag extends EnumEntry with NodeTag
object TypeTag {
  case object ExtensibleRecord extends TypeTag
  case object Function         extends TypeTag
  case object Record           extends TypeTag
  case object Reference        extends TypeTag
  case object Tuple            extends TypeTag
  case object Unit             extends TypeTag
  case object Variable         extends TypeTag
}
