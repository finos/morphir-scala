package org.finos.morphir.runtime
import org.finos.morphir.naming.*

/**
 * Describes the location of a piece of code for error reporting purposes
 */
sealed trait CodeLocation
object CodeLocation {

  /**
   * Whatever IR was passed to the top level of the evaluator
   */
  case object EntryPoint extends CodeLocation

  /**
   * Any function that was defined anonymously, such as lambdas
   *
   * @param outer
   *   The location where this function was defined (if defined in another anonymous function, the first named parent
   *   thereof)
   */
  case class AnonymousFunction(outer: CodeLocation) extends CodeLocation {
    override def toString = s"anonymous function withing $outer"
  }
  object AnonymousFunction {
    def apply(outer: CodeLocation): AnonymousFunction = outer match {
      case AnonymousFunction(parent) =>
        AnonymousFunction(parent) // We want the findable top level, don't care so much how many deep this is
      case _ => new AnonymousFunction(outer)
    }
  }

  /**
   * A function defined at the top level of a module
   *
   * @param name
   *   The fully qualified name of the function
   */
  case class TopLevelFunction(name: FQName) extends CodeLocation {
    override def toString = name.toString
  }

  /**
   * A function from the SDK or another library
   *
   * @param name
   *   The fully qualified name of the function
   */
  case class NativeFunction(name: FQName) extends CodeLocation {
    override def toString = s"$name (native function)"
  }

}
