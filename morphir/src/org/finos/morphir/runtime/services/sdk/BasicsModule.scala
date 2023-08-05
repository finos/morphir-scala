package org.finos.morphir.runtime.services.sdk
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.universe.sdk.*
import _root_.morphir.sdk.Basics
import zio.ZEnvironment
import zio.prelude.fx.ZPure

trait BasicsModule {
  def add[A](l: Basics.Int, r: Basics.Int): URTAction[Basics.Int]
  def add[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float]

//  def modBy(modulus: Basics.Integer, a: Basics.Integer): URTAction[Basics.Integer]

  def subtract[A](l: Basics.Int, r: Basics.Int): URTAction[Basics.Int]
  def subtract[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float]
}
object BasicsModule {
  val live: BasicsModule = BasicsModuleLive()
  val liveEnv: ZEnvironment[BasicsModule] = ZEnvironment[BasicsModule](
    BasicsModule.live
  )
}

final case class BasicsModuleLive() extends BasicsModule {
  def add[A](l: Basics.Int, r: Basics.Int): URTAction[Basics.Int] =
    URTAction.succeed(Basics.add(l, r))

  def add[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float] = ???

//  def modBy(modulus: Basics.Integer, a: Basics.Integer): URTAction[Basics.Integer] =
//    URTAction.succeed(Basics.modBy(modulus, a))

  def subtract[A](l: Basics.Int, r: Basics.Int): URTAction[Basics.Int]       = ???
  def subtract[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float] = ???
}
