package morphir.knowledge.logic.core

import kyo.*

opaque type Flux[A] = FluxEngine.Src[A]

object Flux {
  def empty[A: Tag]: Flux[A] =
    FluxEngine.empty[A]
  def succeed[A: Tag](a: A): Flux[A] =
    FluxEngine.succeed(a)
  def repeat[A: Tag](a: A): Flux[A] =
    FluxEngine.repeat(a)
  def suspend[A: Tag](flux: => Flux[A]): Flux[A] =
    FluxEngine.suspend(flux)
  def mergeAll[A: Tag](streams: Flux[A]*): Flux[A] =
    streams.foldLeft(Flux.empty[A])((acc, flux) => acc.merge(flux))

  extension [A](self: Flux[A])
    def flatMap[B: Tag](f: A => Flux[B])(using Tag[A]): Flux[B] =
      FluxEngine.flatMap(self)(a => f(a))
    def merge[B >: A: Tag](that: Flux[B]): Flux[B] =
      FluxEngine.merge(self, that)

    /**
     * Operator alias for merge.
     */
    def <>[B >: A: Tag](that: Flux[B]): Flux[B] = merge(that)
    def runCollect(using Tag[A]): Chunk[A]      =
      FluxEngine.runCollect(self)
    def runCollectN(n: => Long)(using Tag[A]): Chunk[A] =
      FluxEngine.runCollectN(self, n)
}
