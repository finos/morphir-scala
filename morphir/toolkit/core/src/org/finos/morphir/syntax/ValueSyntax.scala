package org.finos
package morphir
package syntax

import org.finos.morphir.ir.value.recursive.AllValueSyntax

trait ValueSyntax extends AllValueSyntax {
  final val define = morphir.syntax.define
}
