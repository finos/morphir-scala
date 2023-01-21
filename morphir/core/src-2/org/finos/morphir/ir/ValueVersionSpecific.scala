package org.finos
package morphir
package ir

import Name.Name
import Type.Type
trait ValueVersionSpecific {
  final type SpecParameter[+A] = (Name, Type[A])
  object SpecParameter {
    def apply[A](name: Name, tpe: Type[A]): SpecParameter[A]   = (name, tpe)
    def unapply[A](p: SpecParameter[A]): Some[(Name, Type[A])] = Some(p)
  }
  implicit class SpecParameterOps[A](val param: SpecParameter[A]) {
    def map[B](f: A => B): SpecParameter[B] = (param._1, param._2.map(f))
    @inline def name: Name                  = param._1
    @inline def tpe: Type[A]                = param._2
  }
}
