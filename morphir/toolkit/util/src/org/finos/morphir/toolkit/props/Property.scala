package org.finos.morphir.toolkit.props

import izumi.reflect.Tag
sealed class Property[A](val name:String, val initial:A, private val tag: Tag[A]) extends Serializable { self =>
  import Property._
  def :=(value:A):Binding[A]  = Binding(self, value)
  override def equals(that: Any): Boolean = (that: @unchecked) match {
    case that: Property[_] => (name, tag) == ((that.name, that.tag))
  }

  override lazy val hashCode:Int =
    (name + tag).hashCode
}
object Property {
  def apply[A](name: String, initial: A)(implicit tag: Tag[A]): Property[A] =
    new Property(name, initial, tag)

  def unapply[V](property:Property[V]):Some[(String, V)] = Some((property.name, property.initial))

  final case class Binding[V](property:Property[V], value:V)
}

final class Metric[V](name:String, initial:V, val combine: (V,V) => V , tag:Tag[V]) extends Property[V](name, initial, tag)
object Metric {
  def apply[V](name:String, initial:V, combine: (V,V) => V )(implicit tag:Tag[V]):Metric[V] =
    new Metric[V](name, initial, combine, tag)

  def unapply[V](metric: Metric[V]):Some[(String, V, (V,V) => V)] = Some((metric.name, metric.initial, metric.combine))
}


