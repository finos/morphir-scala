package org.finos.morphir.ir.tools

import zio.Chunk
import zio.prelude.fx.ZPure

trait NamesFolder[+W, -SIn, +SOut, -Context, +E, Z] {
  def name(segments: Chunk[String]): ZPure[W, SIn, SOut, Context, E, Z]
  def path(segments: Chunk[Z]): ZPure[W, SIn, SOut, Context, E, Z]
}

object NamesFolder {}


