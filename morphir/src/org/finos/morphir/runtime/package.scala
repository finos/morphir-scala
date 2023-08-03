package org.finos.morphir

package object runtime {
  type URTAction[+A] = RTAction[Any, Nothing, A]
  val URTAction: org.finos.morphir.runtime.RTAction.type = org.finos.morphir.runtime.RTAction

  type ??? = Nothing
}
