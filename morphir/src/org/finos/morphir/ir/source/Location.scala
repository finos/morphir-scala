package org.finos.morphir.ir.source
import zio.prelude._
final case class Location(row: Int, column: Int) {
  def offsetColumnBy(n: Int): Location =
    copy(column = column + n)

  def offsetRowBy(n: Int): Location =
    copy(row = row + n)
}
object Location {
  val default: Location = Location(0, 0)
  val home: Location    = Location(0, 0)

  implicit val LocationIdentity: Identity[Location] = new Identity[Location] {
    final val identity: Location = home

    override def combine(l: => Location, r: => Location): Location =
      Location(row = l.row + r.row, column = l.column + r.column)
  }
}
