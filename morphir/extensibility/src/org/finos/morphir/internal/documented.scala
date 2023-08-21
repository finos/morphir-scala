package org.finos.morphir.internal

trait DocumentedModule {
  sealed case class Documented[+A](doc: String, value: A) {
    def map[B](f: A => B): Documented[B]                 = Documented(doc, f(value))
    def flatMap[B](f: A => Documented[B]): Documented[B] = f(value)
    def zip[B](that: Documented[B]): Documented[(A, B)]  = Documented(doc, (value, that.value))
  }
}
