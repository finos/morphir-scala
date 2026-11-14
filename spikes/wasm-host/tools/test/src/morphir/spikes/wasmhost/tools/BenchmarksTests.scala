package morphir.spikes.wasmhost.tools

import kyo.test.*

class BenchmarksTests extends Test[Any]:
  "independent wrapping-integer and memory references match consumed guest checksums" in {
    assert(Benchmarks.computeChecksum == -2115620968)
    assert(Benchmarks.memoryChecksum == 66846720L)
  }
  "nearest-rank percentiles retain tails and normalize long JSON representations" in {
    val values = Vector(40.0, 10.0, 30.0, 20.0)
    assert(Benchmarks.percentile(values, 0.5) == 20)
    assert(Benchmarks.percentile(values, 0.95) == 40)
    assert(Benchmarks.percentile(Vector(7.0), 0.99) == 7)
    assert(Benchmarks.number(ujson.Str("5000000001")) == 5000000001.0)
  }
