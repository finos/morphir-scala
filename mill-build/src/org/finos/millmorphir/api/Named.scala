package org.finos.millmorphir.api

import upickle.default.*

final case class Named[+A](name: String, value: A) derives ReadWriter {
  def map[B](f: A => B): Named[B]            = Named(name, f(value))
  def flatMap[B](f: A => Named[B]): Named[B] = f(value)
}
