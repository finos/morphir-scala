package org.finos.morphir.toolkit.runtime

trait Resolver extends Product with Serializable

object Resolver {
  val default: Resolver = Default()
  final case class Default() extends Resolver
}
