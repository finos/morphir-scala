package zio

import zio.stream.{ZChannel, ZSink, ZStream}
import zio.stream.ZChannel.{ChildExecutorDecision, UpstreamPullRequest, UpstreamPullStrategy}
import zio._

object ZStreamCompat {
  def flatMapStream[R, R1 <: R, A, B](
      stream: ZStream[R, Nothing, Option[A]]
  )(f: A => ZStream[R1, Nothing, Option[B]])(implicit trace: Trace): ZStream[R1, Nothing, Option[B]] =
    ZStream
      .fromChannel(
        stream
          .rechunk(1)
          .channel
          .concatMapWithCustom[R1, Any, Any, Any, Nothing, Chunk[Either[Boolean, B]], Any, Any](as =>
            as.map {
              case Some(a) =>
                f(a)
                  .rechunk(1)
                  .map {
                    case None    => Left(true)
                    case Some(a) => Right(a)
                  }
                  .channel
              case None =>
                ZStream(Left(false)).channel
            }.fold(ZChannel.unit)(_ *> _)
          )(
            g = (_, _) => (),
            h = (_, _) => (),
            onPull = (request: UpstreamPullRequest[Chunk[Option[A]]]) =>
              request match {
                case UpstreamPullRequest.Pulled(chunk) =>
                  chunk.headOption.flatten match {
                    case Some(_) => UpstreamPullStrategy.PullAfterNext(None)
                    case None =>
                      UpstreamPullStrategy.PullAfterAllEnqueued(None)
                  }
                case UpstreamPullRequest.NoUpstream(activeDownstreamCount) =>
                  UpstreamPullStrategy.PullAfterAllEnqueued[Chunk[Either[Boolean, B]]](
                    if (activeDownstreamCount > 0)
                      Some(Chunk(Left(false)))
                    else
                      None
                  )
              },
            onEmit = (chunk: Chunk[Either[Boolean, B]]) =>
              chunk.headOption match {
                case Some(Left(true)) => ChildExecutorDecision.Yield
                case _                => ChildExecutorDecision.Continue
              }
          )
      )
      .filter(_ != Left(true))
      .map {
        case Left(_)      => None
        case Right(value) => Some(value)
      }
}
