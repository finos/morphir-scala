package org.finos
package morphir
package mir

/**
 * Represents a fresh Local
 */
final class Fresh private (private var start: Long):
  def apply(): Local =
    start += 1
    val value = start
    Local(value)

object Fresh:
  def apply(start: Long = 0L): Fresh =
    new Fresh(start)
