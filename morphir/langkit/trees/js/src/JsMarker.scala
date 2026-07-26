// Platform-folder wiring check: scala.scalajs.js.Dynamic only resolves under ScalaJSModule —
// if this leaked into the jvm/native compile it would fail to resolve there. Delete with the other placeholders.
package morphir.langkit.trees

private[trees] val jsMarker: scala.scalajs.js.Dynamic = scala.scalajs.js.Dynamic.literal()
