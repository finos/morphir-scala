package morphir.mir

import Type._
object Rt:
  val Object: Type.Ref = Ref(Global.Top("java.lang.Object"))
  val Class            = Ref(Global.Top("java.lang.Class"))
  val String           = Ref(Global.Top("java.lang.String"))
  val RuntimeNothing   = Type.Ref(Global.Top("scala.runtime.Nothing$"))
  val RuntimeNull      = Type.Ref(Global.Top("scala.runtime.Null$"))

  val BoxedNull = Ref(Global.Top("scala.runtime.Null$"))
  val BoxedUnit = Ref(Global.Top("scala.runtime.BoxedUnit"))
