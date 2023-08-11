package org.finos.morphir.internal

trait DocumentedModule {
  final case class Documented[+A](doc: String, value: A) {
    def map[B](f: A => B): Documented[B]                 = Documented(doc, f(value))
    def flatMap[B](f: A => Documented[B]): Documented[B] = f(value)
  }
}
