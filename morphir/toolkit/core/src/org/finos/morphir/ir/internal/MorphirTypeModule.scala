package org.finos.morphir.ir.internal

trait MorphirTypeModule extends MorphirTypeModuleBase {
  def typeAttributes[A](tpe: Type[A]): A
  def mapTypeAttributes[A, B](tpe: Type[A])(f: A => B): Type[B]
}
