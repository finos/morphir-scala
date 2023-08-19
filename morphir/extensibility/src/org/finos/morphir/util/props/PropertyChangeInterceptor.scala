package org.finos.morphir.util.props

sealed trait PropertyChangeInterceptor[A] extends ((A, A) => A)
object PropertyChangeInterceptor {

  def apply[A](f: (A, A) => A): PropertyChangeInterceptor[A] = new PropertyChangeInterceptor[A] {
    override def apply(v1: A, v2: A): A = f(v1, v2)
  }

  def KeepNewValue[A]: PropertyChangeInterceptor[A] = PropertyChangeInterceptor((_, newValue) => newValue)

  def KeepOldValue[A]: PropertyChangeInterceptor[A] = PropertyChangeInterceptor((oldValue, _) => oldValue)
}
