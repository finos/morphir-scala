package zio.morphir.mir

sealed trait MorphirIRVersion

object MorphirIRVersion {
  case object V1_0 extends MorphirIRVersion

  val Default: MorphirIRVersion = V1_0
}
