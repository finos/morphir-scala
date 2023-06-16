package morphir.knowledge.logic.core

import zio.Chunk

class FluxSpec extends munit.ZSuite {
  testZ("An empty flux should produce no values") {
    val sut = Flux.empty[Int]
    for {
      res <- sut.runCollect
    } yield assertEquals(res,Chunk.empty)
  }

  testZ("Results are interleaved") {
    val fives = Flux.repeat(5)
    val sixes = Flux.repeat(6)
    val sut   = fives <> sixes
    for {
      res <- sut.runCollectN(10)
    } yield assertEquals(res, Chunk(5, 6, 5, 6, 5, 6, 5, 6, 5, 6))
  }

  testZ("Results are interleaved when using mergeAll") {
    val a_s    = Flux.repeat('a')
    val b_s    = Flux.repeat('b')
    val c_s    = Flux.repeat('c')
    val d_s    = Flux.repeat('d')
    val merged = Flux.mergeAll(a_s, b_s, c_s, d_s)
    for {
      res <- merged.runCollectN(12)
    } yield assertEquals(res, Chunk('a', 'b', 'c', 'd', 'a', 'b', 'c', 'd', 'a', 'b', 'c', 'd'))
  }
}
