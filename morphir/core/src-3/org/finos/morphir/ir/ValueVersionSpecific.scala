package org.finos
package morphir
package ir

import Name.Name
import Type.Type

trait ValueVersionSpecific:
  final opaque type SpecParameter[+A] <: (Name, Type[A]) = (Name, Type[A])

  object SpecParameter:
    def apply[A](name: Name, tpe: Type[A]): SpecParameter[A]   = (name, tpe)
    def unapply[A](p: SpecParameter[A]): Some[(Name, Type[A])] = Some(p)

  extension [A](arg: SpecParameter[A])
    def map[B](f: A => B): SpecParameter[B] = (arg._1, arg._2.map(f))
    def name: Name                          = arg._1
    def tpe: Type[A]                        = arg._2

end ValueVersionSpecific
