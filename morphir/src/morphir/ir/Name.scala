package morphir.ir
import org.finos.morphir.naming

object Name {
  type Name = naming.Name

  def fromString(str: String): Name   = naming.Name.fromString(str)
  def toTitleCase(name: Name): String = name.toTitleCase
  def toCamelCase(name: Name): String = name.toCamelCase
  def toSnakeCase(name: Name): String = name.toSnakeCase
  def toHumanWords(name: Name): List[String] =
    name.humanize

  // TODO: Fill with the rest of the functions available in the morphir-elm project
}
