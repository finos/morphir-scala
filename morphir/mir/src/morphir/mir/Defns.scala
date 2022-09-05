package morphir.mir

sealed abstract class Defn:
  def name:Global
  def attrs:Attrs
  def pos:Position

object Defn:
  final case class TypeAlias(attrs:Attrs, name:Global)(using val pos:Position) extends Defn

end Defn

object Defns:

end Defns
