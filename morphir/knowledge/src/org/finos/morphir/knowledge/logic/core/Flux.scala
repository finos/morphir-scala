package org.finos.morphir.knowledge.logic.core

import zio.stream.ZStream
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
  )(f: A => ZStream[R1, Nothing, Option[B]]): ZStream[R1, Nothing, Option[B]] = {

    sealed trait State

    case object PullOuter extends State
    case class PullInner(
        stream: ZIO[R1, Option[Nothing], Chunk[Option[B]]],
        chunk: Chunk[Option[B]],
        index: Int,
        finalizer: ZManaged.Finalizer
    ) extends State

    def pull(
        outerDone: Ref[Boolean],
        outerStream: ZIO[R, Option[Nothing], Chunk[Option[A]]],
        currentOuterChunk: Ref[(Chunk[Option[A]], Int)],
        currentInnerStream: Ref[Option[PullInner]],
        currentStreams: Ref[ScalaQueue[State]],
        innerFinalizers: ZManaged.ReleaseMap
    ): ZIO[R1, Option[Nothing], Chunk[Option[B]]] = {

      def pullNonEmpty[R, E, A](pull: ZIO[R, Option[E], Chunk[A]]): ZIO[R, Option[E], Chunk[A]] =
        pull.flatMap(as => if (as.nonEmpty) ZIO.succeed(as) else pullNonEmpty(pull))

      def pullOuter(
          outerStream: ZIO[R, Option[Nothing], Chunk[Option[A]]],
          outerChunk: Chunk[Option[A]],
          outerChunkIndex: Int
      ): ZIO[R1, Option[Nothing], (Option[A], Chunk[Option[A]], Int)] =
        if (outerChunkIndex < outerChunk.size)
          ZIO.succeed((outerChunk(outerChunkIndex), outerChunk, outerChunkIndex + 1))
        else
          pullNonEmpty(outerStream).map(chunk => (chunk(0), chunk, 1))

      def openInner(a: A): ZIO[R1, Nothing, (ZIO[R1, Option[Nothing], Chunk[Option[B]]], ZManaged.Finalizer)] =
        ZIO.uninterruptibleMask { restore =>
          for {
            releaseMap <- ZManaged.ReleaseMap.make
            pull       <- restore(f(a).process.zio.provideSome[R1]((_, releaseMap)).map(_._2))
            finalizer  <- innerFinalizers.add(releaseMap.releaseAll(_, ExecutionStrategy.Sequential))
          } yield (pull, finalizer)
        }

      def pullInner(
          innerStream: ZIO[R1, Option[Nothing], Chunk[Option[B]]],
          innerChunk: Chunk[Option[B]],
          innerChunkIndex: Int
      ): ZIO[R1, Option[Nothing], (Option[Chunk[Option[B]]], Chunk[Option[B]], Int)] =
        if (innerChunkIndex < innerChunk.size)
          ZIO.succeed(takeInner(innerChunk, innerChunkIndex))
        else
          pullNonEmpty(innerStream).map(takeInner(_, 0))

      def takeInner(
          innerChunk: Chunk[Option[B]],
          innerChunkIndex: Int
      ): (Option[Chunk[Option[B]]], Chunk[Option[B]], Int) =
        if (innerChunk(innerChunkIndex).isEmpty) {
          (None, innerChunk, innerChunkIndex + 1)
        } else {
          val builder  = ChunkBuilder.make[Option[B]]()
          val length   = innerChunk.length
          var continue = true
          var i        = innerChunkIndex
          while (continue && i != length) {
            val b = innerChunk(i)
            if (b.isDefined) {
              builder += b
              i += 1
            } else {
              continue = false
            }
          }
          (Some(builder.result()), innerChunk, i)
        }

      currentInnerStream.get.flatMap {
        case None =>
          currentStreams.get.map(_.headOption).flatMap {
            case None =>
              ZIO.fail(None)
            case Some(PullInner(innerStream, chunk, index, innerFinalizer)) =>
              currentInnerStream.set(Some(PullInner(innerStream, chunk, index, innerFinalizer))) *>
                currentStreams.update(_.tail) *>
                pull(outerDone, outerStream, currentOuterChunk, currentInnerStream, currentStreams, innerFinalizers)
            case Some(PullOuter) =>
              outerDone.get.flatMap { done =>
                if (done)
                  currentStreams.get.flatMap { queue =>
                    if (queue.size == 1)
                      ZIO.fail(None)
                    else
                      currentStreams.update(_.tail.enqueue(PullOuter)) *>
                        ZIO.succeed(Chunk(None))
                  }
                else
                  currentOuterChunk.get.flatMap { case (outerChunk, outerChunkIndex) =>
                    pullOuter(outerStream, outerChunk, outerChunkIndex).foldM(
                      _ =>
                        outerDone.set(true) *>
                          pull(
                            outerDone,
                            outerStream,
                            currentOuterChunk,
                            currentInnerStream,
                            currentStreams,
                            innerFinalizers
                          ),
                      {
                        case (Some(a), outerChunk, outerChunkIndex) =>
                          openInner(a).flatMap { case (innerStream, innerFinalizer) =>
                            currentOuterChunk.set((outerChunk, outerChunkIndex)) *>
                              currentInnerStream.set(Some(PullInner(innerStream, Chunk.empty, 0, innerFinalizer))) *>
                              pull(
                                outerDone,
                                outerStream,
                                currentOuterChunk,
                                currentInnerStream,
                                currentStreams,
                                innerFinalizers
                              )
                          }
                        case (None, outerChunk, outerChunkIndex) =>
                          currentOuterChunk.set((outerChunk, outerChunkIndex)) *>
                            currentStreams.update(_.tail.enqueue(PullOuter)) *>
                            ZIO.succeed(Chunk(None))
                      }
                    )
                  }
              }
          }
        case Some(PullInner(innerStream, innerChunk, innerChunkIndex, innerFinalizer)) =>
          pullInner(innerStream, innerChunk, innerChunkIndex).foldM(
            _ =>
              innerFinalizer(Exit.unit) *>
                currentInnerStream.set(None) *>
                pull(outerDone, outerStream, currentOuterChunk, currentInnerStream, currentStreams, innerFinalizers),
            {
              case (None, innerChunk, innerChunkIndex) =>
                currentInnerStream.set(None) *>
                  currentStreams.update(
                    _.enqueue(PullInner(innerStream, innerChunk, innerChunkIndex, innerFinalizer))
                  ) *>
                  pull(outerDone, outerStream, currentOuterChunk, currentInnerStream, currentStreams, innerFinalizers)
              case (Some(bs), innerChunk, innerChunkIndex) =>
                currentInnerStream.set(Some(PullInner(innerStream, innerChunk, innerChunkIndex, innerFinalizer))) *>
                  ZIO.succeed(bs)
            }
          )
      }
    }

    ZStream {
      for {
        outerDone          <- Ref.make(false).toManaged_
        outerStream        <- stream.process
        currentOuterChunk  <- Ref.make[(Chunk[Option[A]], Int)]((Chunk.empty, 0)).toManaged_
        currentInnerStream <- Ref.make[Option[PullInner]](None).toManaged_
        currentStreams     <- Ref.make[ScalaQueue[State]](ScalaQueue(PullOuter)).toManaged_
        innerFinalizers    <- ZManaged.ReleaseMap.makeManaged(ExecutionStrategy.Sequential)
      } yield pull(outerDone, outerStream, currentOuterChunk, currentInnerStream, currentStreams, innerFinalizers)
    }
  }

  /**
   * An implementation of `ZStream#merge` that supports breadth first search.
   */
  private def mergeStream[R, A](
      left: ZStream[R, Nothing, Option[A]],
      right: ZStream[R, Nothing, Option[A]]
  ): ZStream[R, Nothing, Option[A]] =
    flatMapStream(ZStream(Some(left), Some(right)))(identity)
}
