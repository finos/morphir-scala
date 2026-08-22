package morphir.langkit.markdown

/**
 * Which of CommonMark's four link forms the author wrote.
 *
 * The AST does not care — every form resolves to a destination and a title — but a CST must say which was written,
 * because the four spell differently: `[t](/u)`, `[t][label]`, `[t][]` and `[t]`.
 */
enum LinkForm derives CanEqual:
  case Inline, ReferenceFull, ReferenceCollapsed, ReferenceShortcut
