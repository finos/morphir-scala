package org.finos.morphir.universe.ir
import enumeratum.values.*
import AccessControlled.*

final case class AccessControlled[+A](access: Access, value: A) { self =>
  def map[B](f: A => B): AccessControlled[B] =
    AccessControlled(access, f(value))

  def flatMap[B](f: A => AccessControlled[B]): AccessControlled[B] =
    f(value)

  def fold[B](ifPublic: A => B, ifPrivate: A => B): B =
    access match {
      case Access.Public  => ifPublic(self.value)
      case Access.Private => ifPrivate(self.value)
    }

  def withPublicAccess: Option[A] = self match {
    case AccessControlled(Access.Public, a) => Some(a)
    case _                                  => None
  }

  /**
   * Get the value with private access level. Will always return the value.
   */
  def withPrivateAccess: A = self match {
    case AccessControlled(Access.Public, a)  => a
    case AccessControlled(Access.Private, a) => a
  }
}

object AccessControlled {
  sealed abstract class Access(val value: Int, val name: String) extends IntEnumEntry

  object Access extends IntEnum[Access] {
    case object Private extends Access(value = 0, name = "private")

    case object Public extends Access(value = 1, name = "public")

    val values = findValues
  }
}
