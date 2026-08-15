package morphir.langkit.markdown

import kyo.*
import kyo.test.*

class FenceInfoTests extends Test[Any]:

  "FenceInfo.parse" - {
    "returns empty for a blank info string" in {
      val info = FenceInfo.parse("")
      assert(info == FenceInfo.empty)
      assert(info.raw.isEmpty)
      assert(info.language.isEmpty)
    }
    "takes the first bare token as language" in {
      val info = FenceInfo.parse("scala")
      assert(info.raw == "scala")
      assert(info.language == Present("scala"))
      assert(info.args.isEmpty)
      assert(info.flags.isEmpty)
      assert(info.attributes.isEmpty)
    }
    "classifies Kyo doctest options and flags" in {
      val info = FenceInfo.parse("scala doctest:expect=runs doctest:scope=env:fixture doctest:setup noformat")
      assert(info.language == Present("scala"))
      assert(info.args == Chunk(
        "doctest:expect=runs",
        "doctest:scope=env:fixture",
        "doctest:setup",
        "noformat"
      ))
      assert(info.option("doctest:expect") == Present("runs"))
      assert(info.option("doctest:scope") == Present("env:fixture"))
      assert(info.flag("doctest:setup"))
      assert(info.flag("noformat"))
      assert(!info.flag("missing"))
    }
    "parses a brace-led Pandoc attribute block" in {
      val info = FenceInfo.parse("{#mycode .haskell .numberLines startFrom=\"100\"}")
      assert(info.raw == "{#mycode .haskell .numberLines startFrom=\"100\"}")
      assert(info.language == Present("haskell"))
      assert(info.id == Present("mycode"))
      assert(info.classes == Chunk("numberLines"))
      assert(info.attributes == Chunk((key = "startFrom", value = "100")))
      assert(info.args.isEmpty)
      assert(info.flags.isEmpty)
    }
    "parses a bare language with a trailing Pandoc brace block" in {
      val info = FenceInfo.parse("scala {.numberLines startFrom=\"100\"}")
      assert(info.language == Present("scala"))
      assert(info.id.isEmpty)
      assert(info.classes == Chunk("numberLines"))
      assert(info.attributes == Chunk((key = "startFrom", value = "100")))
      assert(info.args.isEmpty)
    }
    "keeps raw and partial structure when braces are malformed" in {
      val info = FenceInfo.parse("scala {#broken")
      assert(info.raw == "scala {#broken")
      assert(info.language == Present("scala"))
      assert(info.id.isEmpty)
      assert(info.classes.isEmpty)
    }
  }
