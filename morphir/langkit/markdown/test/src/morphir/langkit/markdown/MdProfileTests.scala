package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.{CstParser, Lower, Parser}

class MdProfileTests extends Test[Any]:
  "MdProfile" - {
    "commonmark recognizes nothing" in assert(MdProfile.commonmark.frontmatter.isEmpty)
    "withYamlFrontmatter enables the yaml kind" in
      assert(MdProfile.commonmark.withYamlFrontmatter.frontmatter == Set(FrontMatterKind.Yaml))
    "the kind carries its delimiter" in assert(FrontMatterKind.Yaml.delimiter == "---")
    "the given default is commonmark" in assert(summon[MdProfile] == MdProfile.commonmark)
    "supportsFrontMatter mirrors the set being non-empty" in {
      assert(!MdProfile.commonmark.supportsFrontMatter)
      assert(MdProfile.commonmark.withYamlFrontmatter.supportsFrontMatter)
    }
    "lowering a CST under any profile agrees with parsing the same source under that profile" in {
      val sources = Chunk("# H\n\npara\n", "- a\n- b\n", "> q\n", "`code`\n", "[l](/u)\n")
      Chunk(MdProfile.commonmark, MdProfile.gfm).foreach { profile =>
        given MdProfile = profile
        sources.foreach { source =>
          val direct  = Parser.parse(source).getOrThrow
          val lowered = Lower.lower(CstParser.parse(source))
          assert(
            direct.unpositioned == lowered.unpositioned,
            s"profile $profile disagrees with itself on ${source.replace("\n", "\\n")}"
          )
        }
      }
    }
  }
