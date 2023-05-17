package morphir.knowledge.logic.core

import zio.stream.{ZChannel, ZSink, ZStream}
import zio._

import scala.collection.immutable.{Queue => ScalaQueue}

final case class Flux[+A](private val stream: ZStream[Any, Nothing, Option[A]]) { self =>

  /**
   * Operator alias for merge.
   */
  @inline def <>[B >: A](that: Flux[B]): Flux[B] = merge(that)

  def flatMap[B](f: A => Flux[B]): Flux[B] =
    Flux(Flux.flatMapStream(self.stream)(a => f(a).stream))

  def merge[B >: A](that: Flux[B]): Flux[B] =
    Flux(Flux.mergeStream(self.stream, that.stream))

  def runCollect: ZIO[Any, Nothing, Chunk[A]] =
    stream.collectSome.runCollect

  def runCollectN(n: Int): ZIO[Any, Nothing, Chunk[A]] =
    stream.collectSome.take(n).runCollect
}

// --5--x--5--x
// --6--x--6--x
// --7--x--7--x

object Flux {
  def empty[A]: Flux[A] =
    Flux(ZStream.empty)
  def mergeAll[A](streams: Flux[A]*): Flux[A] = streams.foldLeft(Flux.empty[A])((acc, flux) => acc <> flux)
  def repeat[A](a: A): Flux[A] =
    Flux(ZStream(Some(a), None).forever)
  def succeed[A](a: A): Flux[A] = Flux(ZStream(Some(a)))
  def suspend[A](flux: => Flux[A]): Flux[A] =
    Flux(ZStream(None) ++ flux.stream)

  /**
   * An implementation of `ZStream#flatMap` that supports breadth first search.
   */
  private def flatMapStream[R, R1 <: R, A, B](
      stream: ZStream[R, Nothing, Option[A]]
  )(f: A => ZStream[R1, Nothing, Option[B]])(implicit trace: Trace): ZStream[R1, Nothing, Option[B]] =
    ZStreamCompat.flatMapStream(stream)(f)

  /**
   * An implementation of `ZStream#merge` that supports breadth first search.
   */
  private def mergeStream[R, A](
      left: ZStream[R, Nothing, Option[A]],
      right: ZStream[R, Nothing, Option[A]]
  ): ZStream[R, Nothing, Option[A]] =
    flatMapStream(ZStream(Some(left), Some(right)))(identity)

}
