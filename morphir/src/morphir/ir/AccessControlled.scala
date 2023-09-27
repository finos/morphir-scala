package morphir.ir

import org.finos.morphir.universe.ir.AccessControlled as AC

object AccessControlled {
  import helpers._

  final type Access = AC.Access
  final val Access: AC.Access.type = AC.Access

  final type AccessControlled[+A] = AC[A]

  object AccessControlled {
    @inline def apply[A](access: Access, value: A): AccessControlled[A] = AC(access, value)

    @inline def unapply[A](value: AccessControlled[A]): Some[(Access, A)] = Some((value.access, value.value))
  }

  def map[A, B](f: A => B): AccessControlled[A] => AccessControlled[B] = ac => ac.map(f)

  @inline def publicAccess[A](value: A): AccessControlled[A] = AC.publicAccess(value)

  @inline def `public`[A](value: A): AccessControlled[A] = AC.privateAccess(value)

  @inline def privateAccess[A](value: A): AccessControlled[A] = AC.privateAccess(value)

  @inline def `private`[A](value: A): AccessControlled[A] = AC.privateAccess(value)

  def withAccess[A](access: Access): WithAccessPartiallyApplied = new WithAccessPartiallyApplied(access)



  final val WithPublicAccess: AC.WithPublicAccess.type = AC.WithPublicAccess
  final val WithPrivateAccess: AC.WithPrivateAccess.type = AC.WithPrivateAccess

  object helpers {
    final class WithAccessPartiallyApplied(val access: Access) extends AnyVal {
      def apply[A](ac: AccessControlled[A]): AccessControlled[A] = ac.copy(access = access)
    }
  }
}
