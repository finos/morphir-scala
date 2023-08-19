package org.finos.morphir.util.props
import izumi.reflect.Tag

sealed class Property[A](
    val name: String,
    val initial: A,
    val propertyChangeInterceptor: PropertyChangeInterceptor[A],
    private val tag: Tag[A]
) extends Serializable { self =>
  import Property._
  def :=(value: A): Binding[A] = Binding(self, value)
  override def equals(that: Any): Boolean = (that: @unchecked) match {
    case that: Property[_] => (name, tag) == ((that.name, that.tag))
  }

  override lazy val hashCode: Int =
    (name + tag).hashCode
}
object Property {

  def apply[A](name: String, initial: A)(implicit tag: Tag[A]): Property[A] =
    new Property(name, initial, PropertyChangeInterceptor.KeepNewValue, tag)

  def apply[A](name: String, initial: A, interceptor: PropertyChangeInterceptor[A])(implicit tag: Tag[A]): Property[A] =
    new Property(name, initial, interceptor, tag)

  /**
   * Alias for `makeMonoidal`.
   */
  def makeMetric[V](name: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): Property[V] =
    makeMonoidal(name, initial, combine)

  def makeMonoidal[V](name: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): Property[V] =
    new Property[V](name, initial, PropertyChangeInterceptor(combine), tag)

  def unapply[V](property: Property[V]): Some[(String, V, PropertyChangeInterceptor[V])] = Some(
    (property.name, property.initial, property.propertyChangeInterceptor)
  )

  final case class Binding[V](property: Property[V], value: V)
}

sealed abstract case class PropertyValue[V] private (value: V, tag: Tag[V])
object PropertyValue {
  def apply[V](value: V)(implicit tag: Tag[V]): PropertyValue[V] = new PropertyValue(value, tag) {}
}
