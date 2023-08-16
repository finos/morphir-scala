//// poor man's enum that works in Scala 2
//package wheels.enums
//
//import org.finos.morphir.meta._
//
//trait Enum[A] { self =>
//  def values: List[A]
//
//  final def map[B](f: A => B): Enum[B] = new Enum[B] {
//    def values: List[B] = self.values.map(f)
//  }
//}
//object Enum extends GeneratedEnums {
//  def apply[A](implicit a: Enum[A]): Enum[A] = a
//
//  // allows us to write Enum.derived[Foo].values, a slight divergence from the norm
//  def derived[A]: Derived[A] = new Derived[A]
//  final class Derived[A] {
//    def values[B](implicit I: Shapely[A, B], B: Enum[B]): List[A] = B.map(I.from).values
//  }
//
//}
