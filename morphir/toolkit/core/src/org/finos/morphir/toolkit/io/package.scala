package org.finos.morphir
package toolkit
package object io {
  type MorphirTypeWriter[-Context] = org.finos.morphir.ir.io.TypeWriter[Context, Attributes]
  // val MorphirTypeWriter = org.finos.morphir.ir.io.MorphirTypeWriter
}
