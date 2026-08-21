package morphir.langkit.markdown

/** The text of a YAML document, as frontmatter carries it: raw, undecoded. Extension home for YAML helpers later. */
opaque type YamlDocText = String

object YamlDocText:
  def apply(value: String): YamlDocText            = value
  given CanEqual[YamlDocText, YamlDocText]         = CanEqual.derived
  extension (text: YamlDocText) def unwrap: String = text
