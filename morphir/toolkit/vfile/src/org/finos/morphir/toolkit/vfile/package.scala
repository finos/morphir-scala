package org.finos.morphir.toolkit

package object vfile {
  type VFileRepr[+A] = VFile.Repr[A]
  val VFileRepr = VFile.Repr
}
