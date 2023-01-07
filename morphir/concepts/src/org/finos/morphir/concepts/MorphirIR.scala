package org.finos
package morphir
package concepts

import core.types.Name
object MorphirIR:
  sealed trait Tree extends Product with Serializable
  sealed trait Defn extends Tree
  sealed trait Spec extends Tree

  enum Distro:
    case Library
    case Bundle

  case class ModuleDef()  extends Defn
  case class ModuleSpec() extends Spec
  enum Type[+A] extends Tree:
    case Unit(attributes: A)
    case Variable(attributes: A, name: Name)

  enum Value[+TA, +VA] extends Tree:
    case Unit(attributes: VA)
