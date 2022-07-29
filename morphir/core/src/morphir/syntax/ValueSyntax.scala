package morphir.syntax

import morphir.mir.value.recursive.AllValueSyntax

trait ValueSyntax extends AllValueSyntax {
  final val define = morphir.syntax.define
}
