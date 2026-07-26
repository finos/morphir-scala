// Platform-folder wiring check: scala.scalanative.unsafe only resolves under ScalaNativeModule —
// if this leaked into the jvm/js compile it would fail to resolve there. Delete with the other placeholders.
package morphir.langkit.trees

private[trees] val nativeMarker: Int = scala.scalanative.unsafe.sizeOf[Int]
