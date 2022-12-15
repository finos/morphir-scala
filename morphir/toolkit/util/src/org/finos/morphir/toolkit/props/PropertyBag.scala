package org.finos.morphir.toolkit.props

import org.finos.morphir.toolkit.props.Property.Binding

final class PropertyBag private (private val map: Map[Property[Any], AnyRef]) {
  self =>
  def ++=(bindings: Seq[Binding[_]]): PropertyBag = new PropertyBag(
    (self.map.toVector ++ bindings.map(b => b.property.asInstanceOf[Property[Any]] -> b.value.asInstanceOf[AnyRef]))
      .foldLeft[Map[Property[Any], AnyRef]](Map()) { case (acc, (property, value)) =>
        acc.updated(
          property,
          acc.get(property).fold(value)(property.propertyChangeInterceptor(_, value).asInstanceOf[AnyRef])
        )
      }
  )
  def get[V](property: Property[V]): V =
    map.get(property.asInstanceOf[Property[Any]]).fold(property.initial)(_.asInstanceOf[V])

  def hasProperty[V](property: Property[V]): Boolean = map.contains(property.asInstanceOf[Property[Any]])

  private def overwrite[V](property: Property[V], value: V): PropertyBag =
    new PropertyBag(map.updated(property.asInstanceOf[Property[Any]], value.asInstanceOf[AnyRef]))

  def update[V](property: Property[V], f: V => V): PropertyBag =
    overwrite(property, f(get(property)))

  def set[V](property: Property[V], value: V): PropertyBag =
    update[V](property, property.propertyChangeInterceptor(_, value))
}

object PropertyBag {
  val empty: PropertyBag = new PropertyBag(Map.empty)
  def apply(bindings: Binding[_]*): PropertyBag =
    empty ++= bindings
}
