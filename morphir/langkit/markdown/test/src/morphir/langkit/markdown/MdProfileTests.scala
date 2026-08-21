package morphir.langkit.markdown

import kyo.*
import kyo.test.*

class MdProfileTests extends Test[Any]:
  "MdProfile" - {
    "commonmark recognizes nothing" in assert(MdProfile.commonmark.frontmatter.isEmpty)
    "withYamlFrontmatter enables the yaml kind" in
      assert(MdProfile.commonmark.withYamlFrontmatter.frontmatter == Set(FrontMatterKind.Yaml))
    "the kind carries its delimiter" in assert(FrontMatterKind.Yaml.delimiter == "---")
    "the given default is commonmark" in assert(summon[MdProfile] == MdProfile.commonmark)
  }
