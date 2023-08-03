package org.finos.morphir.runtime.services.sdk
import org.finos.morphir.runtime.*
import org.finos.morphir.universe.sdk.*
import org.finos.morphir.universe.sdk.Basics
import org.finos.morphir.universe.sdk.Basics.*

import zio.prelude.fx.ZPure

trait BasicsModule {
  def add[A](l: Basics.Integer, r: Basics.Integer): URTAction[Basics.Integer]
  def add[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float]

  def subtract[A](l: Basics.Integer, r: Basics.Integer): URTAction[Basics.Integer]
  def subtract[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float]
}
object BasicsModule {
  val live: BasicsModule = BasicsModuleLive()
}

final case class BasicsModuleLive() extends BasicsModule {
  import _root_.morphir.sdk.Basics

  def add[A](l: Basics.Integer, r: Basics.Integer): URTAction[Basics.Integer] =
    URTAction.succeed(Basics.add(l, r))

  def add[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float] = ???

  def subtract[A](l: Basics.Integer, r: Basics.Integer): URTAction[Basics.Integer] = ???
  def subtract[A](l: Basics.Float, r: Basics.Float): URTAction[Basics.Float]       = ???
}
