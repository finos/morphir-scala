package org.finos.morphir
package ir
package io

import org.finos.morphir.naming._
import zio._

trait TypeWriter[-Context, -Attributes] {

  def writeAttributes(context: Context, attributes: Attributes): Unit

  def writeAttributesZIO[Ctx <: Context](attributes: Attributes)(implicit tag: Tag[Ctx]): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeAttributes(context, attributes)))

  def writeName(context: Context, name: Name): Unit
  def writeNameZIO[Ctx <: Context: Tag](name: Name): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeName(context, name)))

  def writeVariable(context: Context, attributes: Attributes, name: Name): Unit
  def writeVariableZIO[Ctx <: Context: Tag](attributes: Attributes, name: Name): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeVariable(context, attributes, name)))

  def writeUnit(context: Context, attributes: Attributes): Unit

  def writeUnitZIO[Ctx <: Context: Tag](attributes: Attributes): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeUnit(context, attributes)))
}
