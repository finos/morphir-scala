package org.finos.morphir
package ir
package types.folding
import zio.Chunk
sealed trait Type[+A] {}
object Type {
  trait Folder[-Context, -Attrib, Z] {
    def reference(context: Context, attributes: Attrib, name: FQName, typeParams: Chunk[Z]): Z
    def tuple(context: Context, attributes: Attrib, elements: Chunk[Z]): Z
    def unit(context: Context, attributes: Attrib): Z
    def variable(context: Context, attributes: Attrib, name: Name): Z
  }
}
