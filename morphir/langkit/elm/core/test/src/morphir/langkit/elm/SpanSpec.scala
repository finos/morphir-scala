package morphir.langkit.elm

import kyo.test.*

class SpanTest extends Test[Any]:

  "Span" - {
    "represents the empty span at offset zero" in
      assert(Span.zero == Span(0, 0))
    "computes the end offset" in
      assert(Span(3, 7).end == 10)
    "spans from the first offset through the second end" in {
      val a = Span(2, 3)
      val b = Span(10, 2)
      assert(Span.between(a, b) == Span(2, 10))
    }
  }
