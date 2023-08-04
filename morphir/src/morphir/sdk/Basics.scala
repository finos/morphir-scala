package morphir.sdk
import org.finos.morphir.extensibility.*

object Basics extends MorphirSdkModule("morphir.sdk", "Basics") {

  type Integer = org.finos.morphir.universe.sdk.Basics.Integer
  private val Integer: org.finos.morphir.universe.sdk.Basics.Integer.type =
    org.finos.morphir.universe.sdk.Basics.Integer

  type Int = Integer

  type Float = org.finos.morphir.universe.sdk.types.MFloat
  private val Float: org.finos.morphir.universe.sdk.types.MFloat.type =
    org.finos.morphir.universe.sdk.Basics.Float

  def add(a: Int): Int => Int          = b => a add b
  def add(a: Int, b: Int): Int         = Integer(a.value + b.value)
  def modBy(modulus: Int): Int => Int  = x => Integer(x % modulus)
  def modBy(modulus: Int, a: Int): Int = Integer(a % modulus)

  /// A "Boolean" value. It can either be `True` or `False`.
  type Bool = scala.Boolean
  val True: Bool                          = true
  val False: Bool                         = false
  @inline def not(a: Bool): Bool          = !a
  @inline def and(a: Bool): Bool => Bool  = b => a && b
  @inline def and(a: Bool, b: Bool): Bool = a && b
  @inline def or(a: Bool): Bool => Bool   = b => a || b
  @inline def or(a: Bool, b: Bool): Bool  = a || b
  @inline def xor(a: Bool): Bool => Bool  = b => a ^ b
  @inline def xor(a: Bool, b: Bool): Bool = a ^ b

  // Equality
  @inline def equal[A](a: A): A => Bool     = b => a == b
  @inline def equal[A](a: A, b: A): Bool    = a == b
  @inline def notEqual[A](a: A): A => Bool  = b => a != b
  @inline def notEqual[A](a: A, b: A): Bool = a != b

  // COMPARISONS

  sealed abstract class Order(val value: scala.Int) extends Product with Serializable
  object Order {
    case object LT extends Order(-1)
    case object EQ extends Order(0)
    case object GT extends Order(1)
  }

  val LT: Order = Order.LT
  val EQ: Order = Order.EQ
  val GT: Order = Order.GT
}
