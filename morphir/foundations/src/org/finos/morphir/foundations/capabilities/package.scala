package org.finos.morphir.foundations

package object capabilities {
  implicit class ShowOps[A](val self: A) extends AnyVal {
    def show(implicit ev: Show[A]): String = ev.show(self)
  }
}
