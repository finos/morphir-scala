package org.finos.morphir
package ir
package io

import org.finos.morphir.naming._
import zio._

trait TypeWriter[-Context, -Attribs] {

  def writeAttributes(context: Context, attributes: Attribs): Unit

  def writeAttributesZIO[Ctx <: Context](attributes: Attribs)(implicit tag: Tag[Ctx]): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeAttributes(context, attributes)))

  def writeName(context: Context, name: Name): Unit
  def writeNameZIO[Ctx <: Context: Tag](name: Name): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeName(context, name)))

  def writeVariable(context: Context, attributes: Attribs, name: Name): Unit
  def writeVariableZIO[Ctx <: Context: Tag](attributes: Attribs, name: Name): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeVariable(context, attributes, name)))

  def writeUnit(context: Context, attributes: Attribs): Unit

  def writeUnitZIO[Ctx <: Context: Tag](attributes: Attribs): ZIO[Ctx, Throwable, Unit] =
    ZIO.serviceWithZIO[Ctx](context => ZIO.attempt(writeUnit(context, attributes)))
}
