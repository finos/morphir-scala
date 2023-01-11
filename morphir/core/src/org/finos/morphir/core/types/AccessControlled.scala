package org.finos.morphir.core.types

sealed trait AccessControlled[+A] { self =>
  import AccessControlled.*
  def value: A
  def access: Access =
    self match {
      case Public(_)  => Access.Public
      case Private(_) => Access.Private
    }

  @inline def fold[Z](whenPrivate: A => Z, whenPublic: A => Z): Z =
    self match {
      case Private(v) => whenPrivate(v)
      case Public(v)  => whenPublic(v)
    }

  def map[B](f: A => B): AccessControlled[B] =
    self match {
      case Public(v)  => Public(f(v))
      case Private(v) => Private(f(v))
    }
}

object AccessControlled {
  import enumeratum.values.*
  final case class Public[A](value: A)  extends AccessControlled[A]
  final case class Private[A](value: A) extends AccessControlled[A]

  def withPublicAccess[A](ac: AccessControlled[A]): Option[A] =
    ac match {
      case Public(a) => Some(a)
      case _         => None
    }

  def withPrivateAccess[A](ac: AccessControlled[A]): Option[A] =
    ac match {
      case Private(a) => Some(a)
      case _          => None
    }

  def withAccess[A](access: Access): AccessControlled[A] => Option[A] = ac =>
    if (ac.access == access) Some(ac.value)
    else None

  sealed abstract class Access(val value: Int, val name: String) extends IntEnumEntry

  object Access extends IntEnum[Access] {
    case object Private extends Access(value = 0, name = "private")

    case object Public extends Access(value = 1, name = "public")

    val values = findValues
  }
}
