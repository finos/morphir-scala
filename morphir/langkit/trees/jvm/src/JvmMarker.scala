// Platform-folder wiring check: java.lang.management is real-JVM-only — if this leaked
// into the js/native compile it would fail to resolve there. Delete with the other placeholders.
package morphir.langkit.trees

private[trees] val jvmMarker: Class[_] = classOf[java.lang.management.ManagementFactory]
