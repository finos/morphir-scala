package org.finos.morphir.datamodel.classic

case class Person(fName: String, lName: Int)
object Joe {
  def unapply(p: Person) =
    if (p.fName == "Joe") Some((p.fName, p.lName))
    else None
}
