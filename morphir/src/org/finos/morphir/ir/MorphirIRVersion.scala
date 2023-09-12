package org.finos.morphir
package ir

sealed abstract class MorphirIRVersion(version: String) extends Product with Serializable {
  final val versionNumber: String = version
}

object MorphirIRVersion {
  case object V1_0 extends MorphirIRVersion("1.0")
  case object V2_0 extends MorphirIRVersion("2.0")
  case object V3_0 extends MorphirIRVersion("3.0")

  val Default: MorphirIRVersion = V3_0
}
