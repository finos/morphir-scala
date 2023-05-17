package zio

import zio.stream.{ZChannel, ZSink, ZStream}
import scala.collection.immutable.Queue as ScalaQueue
import zio._

object ZStreamCompat {
  def flatMapStream[R, R1 <: R, A, B](
      stream: ZStream[R, Nothing, Option[A]]
  )(f: A => ZStream[R1, Nothing, Option[B]])(implicit trace: Trace): ZStream[R1, Nothing, Option[B]] = {
    sealed trait State

    case object PullOuter extends State
    case class PullInner(
        stream: ZIO[R1, Option[Nothing], Chunk[Option[B]]],
        chunk: Chunk[Option[B]],
        index: Int,
        finalizer: Exit[Any, Any] => UIO[Any]
    ) extends State

    def pull(
        outerDone: Ref[Boolean],
        outerStream: ZIO[R, Option[Nothing], Chunk[Option[A]]],
        currentOuterChunk: Ref[(Chunk[Option[A]], Int)],
        currentInnerStream: Ref[Option[PullInner]],
        currentStreams: Ref[ScalaQueue[State]],
        innerFinalizers: Scope
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

      def openInner(a: A): ZIO[R1, Nothing, (ZIO[R1, Option[Nothing], Chunk[Option[B]]], Exit[Any, Any] => UIO[Any])] =
        for {
          scope <- innerFinalizers.fork
          pull  <- scope.extend[R1](f(a).toPull)
        } yield (pull, scope.close(_))

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
                    pullOuter(outerStream, outerChunk, outerChunkIndex).foldZIO(
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
          pullInner(innerStream, innerChunk, innerChunkIndex).foldZIO(
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

    ZStream.fromPull[R1, Nothing, Option[B]] {
      for {
        outerDone          <- Ref.make(false)
        outerStream        <- stream.toPull
        currentOuterChunk  <- Ref.make[(Chunk[Option[A]], Int)]((Chunk.empty, 0))
        currentInnerStream <- Ref.make[Option[PullInner]](None)
        currentStreams     <- Ref.make[ScalaQueue[State]](ScalaQueue(PullOuter))
        innerFinalizers    <- Scope.make
      } yield pull(outerDone, outerStream, currentOuterChunk, currentInnerStream, currentStreams, innerFinalizers)
    }
  }
}
