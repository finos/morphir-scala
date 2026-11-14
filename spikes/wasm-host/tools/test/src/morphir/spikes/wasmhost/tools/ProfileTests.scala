package morphir.spikes.wasmhost.tools

import kyo.test.*

class ProfileTests extends Test[Any]:
  "normalizes KiB, deduplicates PIDs and excludes unrequested processes" in {
    val rss = Profile.parseRss("12 10\n12 9\n13 20\n99 10000\n", Set(12L, 13L))
    assert(rss == Map(12L -> 10240L, 13L -> 20480L))
    assert(Profile.aggregate(Set(12L, 13L), rss).num == 30720)
  }
  "keeps absent measurements unavailable" in {
    assert(Profile.aggregate(Set(12L, 13L), Map(12L -> 1024L)) == ujson.Null)
    assert(Profile.aggregate(Set.empty, Map.empty) == ujson.Null)
  }
  "aggregate peak comes from a concurrent sum, not independent process peaks" in {
    val samples = Seq(Map(12L -> 100L, 13L -> 10L), Map(12L -> 10L, 13L -> 100L))
    val peak    = samples.map(Profile.aggregate(Set(12L, 13L), _).num).max
    assert(peak == 110)
  }
