package org.finos
package morphir.mir

enum Spec:
  case Func

  final def mangle: String = Mangle(this)
  final def show: String   = Show(this)
