package org.finos.morphir.knowledge.logic.core

import zio.Chunk
import zio.test._
import zio.test.Assertion._

object FluxSpec extends ZIOSpecDefault {
  override def spec = suite("FluxSpec")(
    test("An empty flux should produce no values") {
      val sut = Flux.empty[Int]
      for {
        res <- sut.runCollect
      } yield assert(res)(equalTo(Chunk.empty))
    },
    test("Results are interleaved") {
      val fives = Flux.repeat(5)
      val sixes = Flux.repeat(6)
      val sut   = fives <> sixes
      for {
        res <- sut.runCollectN(10)
      } yield assert(res)(equalTo(Chunk(5, 6, 5, 6, 5, 6, 5, 6, 5, 6)))
    },
    test("Results are interleaved when using mergeAll") {
      val a_s    = Flux.repeat('a')
      val b_s    = Flux.repeat('b')
      val c_s    = Flux.repeat('c')
      val d_s    = Flux.repeat('d')
      val merged = Flux.mergeAll(a_s, b_s, c_s, d_s)
      for {
        res <- merged.runCollectN(12)
      } yield assert(res)(equalTo(Chunk('a', 'b', 'c', 'd', 'a', 'b', 'c', 'd', 'a', 'b', 'c', 'd')))
    }
  )
}
