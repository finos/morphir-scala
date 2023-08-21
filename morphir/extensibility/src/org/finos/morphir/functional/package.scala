package org.finos.morphir

package object functional {
  type Id[+A] = Id.Type[A]
  object Id extends IdNewtypeEncoding {}
}
