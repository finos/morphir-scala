package org.finos
package morphir

package object util {
  def unreachable: Nothing =
    throw UnreachableException

  def unsupported(v: Any): Nothing =
    throw UnsupportedException(s"$v (${v.getClass})")

  def unsupported(s: String = ""): Nothing =
    throw UnsupportedException(s)

  type UnreachableException = UnreachableException.type
}
