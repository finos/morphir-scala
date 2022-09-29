package org.finos.morphir.ir.tools

import zio.Chunk
import zio.prelude.fx.ZPure
import io.lemonlabs.uri.Urn
trait NameFolder[+W, -SIn, +SOut, -Context, +E, +Z] {
  def name(segments: Chunk[String]): ZPure[W, SIn, SOut, Context, E, Z]
}

object NameFolder {
  def make[W, SIn, SOut, Context, E, Z](
      f: Chunk[String] => ZPure[W, SIn, SOut, Context, E, Z]
  ): NameFolder[W, SIn, SOut, Context, E, Z] =
    new NameFolder[W, SIn, SOut, Context, E, Z] {
      def name(segments: Chunk[String]): ZPure[W, SIn, SOut, Context, E, Z] =
        f(segments)
    }

  def encoder[Z](f: Chunk[String] => Z): NameFolder[Nothing, Unit, Unit, Any, Nothing, Z] =
    new NameFolder[Nothing, Unit, Unit, Any, Nothing, Z] {
      override def name(segments: Chunk[String]): ZPure[Nothing, Unit, Unit, Any, Nothing, Z] =
        ZPure.succeed(f(segments))
    }

  def urnEncoder(f: Chunk[String] => Urn): NameFolder[Nothing, Unit, Unit, Any, Nothing, Urn] = encoder(f)
  def urnEncoder: NameFolder[Nothing, Unit, Unit, Any, Nothing, Urn] =
    urnEncoder(segments => Urn.apply("local-name", segments.map(_.toLowerCase).mkString("-")))
}
