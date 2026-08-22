package morphir.langkit.markdown

import java.util.Locale
import kyo.test.*
import morphir.langkit.markdown.internal.HtmlTag

/**
 * Tag matching must not read the default locale.
 *
 * `String.toLowerCase` with no locale argument folds against the JVM's default one, and Turkish maps `I` to the dotless
 * `ı` rather than to `i`: `"TITLE".toLowerCase` is `"tıtle"` there, which matches no name in the disallowed roster and
 * would let `<TITLE>` through unescaped. The same trap sat under the HTML block start conditions, where `<SCRIPT>`
 * would stop opening a script-like block. Tag names are ASCII by the spec's own grammar, so the fold the production
 * code uses reads no locale at all.
 *
 * This suite deliberately does NOT call `Locale.setDefault`. The default locale is process state shared by every suite
 * in the module's test JVM, and a first version of this test flipped it to Turkish around a parse — which
 * intermittently broke unrelated suites running at the same time: kyo's `Flag` uppercases a flag name through the
 * default locale, so `morphir.conformance.fixtures` became `MORPHİR_CONFORMANCE_FİXTURES` mid-window and the
 * conformance corpus read as missing. The failure CI saw was exactly that race. Instead, this suite pins the two halves
 * separately: the Turkish fold really does corrupt ASCII names (so the hazard is real, not hypothetical), and the fold
 * the production code uses is byte-stable regardless of any locale. The end-to-end path over `<TITLE>` and `<Iframe>`
 * is covered in `TagFilterTests` under the ambient locale, where it exercises the same `asciiLower` calls this suite
 * proves locale-free.
 *
 * JVM-only because `java.util.Locale` is where the hazard lives; the fold is the same code on all three platforms.
 */
class TagFilterLocaleTests extends Test[Any]:

  private val turkish = Locale.forLanguageTag("tr")

  "locale-free tag folding" - {

    "the Turkish fold corrupts ASCII tag names, which is the hazard being defended" in {
      assert("TITLE".toLowerCase(turkish) == "tıtle")
      assert("SCRIPT".toLowerCase(turkish) == "scrıpt")
    }

    "the production fold is exact for ASCII whatever any locale says" in {
      assert(HtmlTag.asciiLower("TITLE") == "title")
      assert(HtmlTag.asciiLower("SCRIPT") == "script")
      assert(HtmlTag.asciiLower("Iframe") == "iframe")
      // Non-ASCII passes through untouched rather than being case-folded: a tag name containing one matches
      // nothing in the roster either way, and folding it would be the locale-dependence this fold exists to avoid.
      assert(HtmlTag.asciiLower("İframe") == "İframe")
    }
  }
