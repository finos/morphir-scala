package morphir.langkit.core.scanner

import kyo.test.*
import morphir.langkit.core.Span
import scala.language.strictEquality

class SourceViewTests extends Test[Any]:

  private def rejects(thunk: => Any): Boolean =
    try
      thunk
      false
    catch case _: IllegalArgumentException => true

  private def rejectsIndex(thunk: => Any): Boolean =
    try
      thunk
      false
    catch case _: IndexOutOfBoundsException => true

  "SourceView" - {
    "retains its source and exposes typed boundaries without materializing text" in {
      val source = "alpha beta"
      val span   = Span(6, 4)
      val view   = SourceView.fromSpan(source, span)

      assert(view.source.eq(source))
      assert(view.span == span)
      assert(view.start == SourceOffset(6))
      assert(view.end == SourceOffset(10))
      assert(view.length == CodeUnitCount(4))
      assert(view.charAt(CodeUnitCount(0)) == 'b')
      assert(view.charAt(CodeUnitCount(3)) == 'a')
      assert(view.text == "beta")
    }

    "accepts an empty view at the end of the source" in {
      val view = SourceView.fromSpan("alpha", Span(5, 0))

      assert(view.isEmpty)
      assert(!view.nonEmpty)
      assert(view.text.isEmpty)
    }

    "rejects a negative span offset" in
      assert(rejects(SourceView.fromSpan("alpha", Span(-1, 1))))

    "rejects a negative span length" in
      assert(rejects(SourceView.fromSpan("alpha", Span(0, -1))))

    "rejects a span extending beyond the source" in
      assert(rejects(SourceView.fromSpan("alpha", Span(4, 2))))

    "rejects a relative offset exactly at the view length" in {
      val view = SourceView.fromSpan("alpha beta", Span(6, 4))
      assert(rejectsIndex(view.charAt(CodeUnitCount(4))))
    }

    "uses UTF-16 code units for lengths and character access" in {
      val source = "\ud83d\ude00"
      val view   = SourceView.fromSpan(source, Span(0, 2))

      assert(view.length == CodeUnitCount(2))
      assert(view.charAt(CodeUnitCount(0)) == source.charAt(0))
      assert(view.charAt(CodeUnitCount(1)) == source.charAt(1))
      assert(view.charAt(CodeUnitCount(0)).isHighSurrogate)
      assert(view.charAt(CodeUnitCount(1)).isLowSurrogate)
    }

    "supports strict equality for equivalent views" in {
      val source = "alpha"
      assert(SourceView.fromSpan(source, Span(1, 3)) == SourceView.fromSpan(source, Span(1, 3)))
    }
  }
