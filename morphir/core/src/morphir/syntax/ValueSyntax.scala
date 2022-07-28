package morphir.syntax

import morphir.ir.value.recursive.AllValueSyntax

trait ValueSyntax extends AllValueSyntax {
  final val define = morphir.syntax.define
}
