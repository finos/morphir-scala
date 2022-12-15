package org.finos.morphir.toolkit.props

final class PropertyBag private (private val map: Map[Property[Any], AnyRef]) {
  self =>
  import Property.Binding
  def ++=(bindings: Seq[Binding[_]]):PropertyBag = new PropertyBag(
    (self.map.toVector ++ bindings.map(b => b.property.asInstanceOf[Property[Any]] -> b.value.asInstanceOf[AnyRef] )).foldLeft[Map[Property[Any], AnyRef]](Map()){
      case (acc, (key @ Metric(_,_,_), value)) =>
        acc.updated(key, acc.get(key).fold(value)(key.combine(_,value).asInstanceOf[AnyRef]))
      case (acc, (key, value)) =>
        acc.updated(key, value)
    }
  )
  def get[V](property: Property[V]): V =
    map.get(property.asInstanceOf[Property[Any]]).fold(property.initial)(_.asInstanceOf[V])

  def hasProperty[V](property: Property[V]): Boolean = map.contains(property.asInstanceOf[Property[Any]])

  private def overwrite[V](property:Property[V], value:V):PropertyBag =
    new PropertyBag(map.updated(property.asInstanceOf[Property[Any]], value.asInstanceOf[AnyRef]))

  def update[V](property:Property[V], f: V => V):PropertyBag =
    overwrite(property, f(get(property)))
}

object PropertyBag {
  val empty:PropertyBag = new PropertyBag(Map.empty)
}
