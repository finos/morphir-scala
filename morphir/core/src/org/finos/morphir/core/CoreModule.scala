package org.finos.morphir
package core

trait CoreModule:
  type TypeAttribs
  type ValueAttribs

  export internal.DistroVisitor
