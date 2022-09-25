package org.finos
package morphir
package mir

private[mir] object Locals:
  opaque type Local = Long

  object Local:
    def apply(id: Long): Local                     = id
    inline def unapply(value: Local): Option[Long] = Some(value)

  extension (local: Local)
    def id: Long     = local
    def show: String = Show(local)
