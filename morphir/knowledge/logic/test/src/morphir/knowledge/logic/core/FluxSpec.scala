package morphir.knowledge.logic.core

import kyo.*
import kyo.test.*

class FluxSpec extends Test[Any]:
  "An empty flux should produce no values" in {
    val sut = Flux.empty[Int]
    val res = sut.runCollect
    assert(res == Chunk.empty)
  }

  "Results are interleaved" in {
    val fives = Flux.repeat(5)
    val sixes = Flux.repeat(6)
    val sut   = fives <> sixes
    val res   = sut.runCollectN(10)
    assert(res == Chunk(5, 6, 5, 6, 5, 6, 5, 6, 5, 6))
  }

  "Results are interleaved when using mergeAll" in {
    val a_s    = Flux.repeat('a')
    val b_s    = Flux.repeat('b')
    val c_s    = Flux.repeat('c')
    val d_s    = Flux.repeat('d')
    val merged = Flux.mergeAll(a_s, b_s, c_s, d_s)
    val res    = merged.runCollectN(12)
    assert(res == Chunk('a', 'b', 'c', 'd', 'a', 'b', 'c', 'd', 'a', 'b', 'c', 'd'))
  }
end FluxSpec
