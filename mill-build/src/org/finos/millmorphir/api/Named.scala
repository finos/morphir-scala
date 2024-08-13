package org.finos.millmorphir.api

final case class Named[+A](name:String, value:A) {
  def map[B](f: A => B): Named[B] = Named(name, f(value))
  def flatMap[B](f: A => Named[B]): Named[B] = f(value)
}

object Named {
  implicit def jsonFormatter[A: upickle.default.ReadWriter]: upickle.default.ReadWriter[Named[A]] = upickle.default.macroRW
}