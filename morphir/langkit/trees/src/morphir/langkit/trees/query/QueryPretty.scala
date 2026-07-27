package morphir.langkit.trees.query

import hearth.kindlings.fastshowpretty.FastShowPretty
import hearth.kindlings.fastshowpretty.RenderConfig

import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName

object QueryPretty:

  def render(query: Query): String =
    render(query, RenderConfig.Compact)

  def render(query: Query, config: RenderConfig): String =
    val sb = new StringBuilder
    queryFastShowPretty.render(sb, config, config.startLevel)(query).toString

  given queryFastShowPretty: FastShowPretty[Query] with

    def render(sb: StringBuilder, config: RenderConfig, level: Int)(query: Query): StringBuilder =
      val roots = query.root match
        case MultiPattern(patterns) => patterns
        case root                   => List(root)
      appendTopLevel(sb, roots.map(renderPattern) ++ query.predicates.map(renderPredicate), config, level)

  private def appendTopLevel(
      sb: StringBuilder,
      fragments: List[String],
      config: RenderConfig,
      level: Int
  ): StringBuilder =
    val indent = config.indentString * level
    fragments.zipWithIndex.foreach { case (fragment, idx) =>
      if idx > 0 then sb.append('\n')
      sb.append(indent).append(fragment)
    }
    sb

  private def renderPattern(pattern: Pattern): String = pattern match
    case NodePattern(nodeType, fields, children, capture, anchors, negatedFields, childQuantifiers) =>
      val parts = collection.mutable.ArrayBuffer.empty[String]
      parts += NodeTypeName.unwrap(nodeType)
      parts ++= fields.map(renderFieldPattern)
      parts ++= negatedFields.toList.sortBy(FieldName.unwrap).map(f => s"!${FieldName.unwrap(f)}")
      children.zipWithIndex.foreach { case (child, idx) =>
        parts += renderPattern(child) + renderQuantifier(childQuantifiers.get(idx))
        if idx < children.size - 1 && anchors.contains(idx) then parts += "."
      }
      val rendered = s"(${parts.mkString(" ")})"
      capture.fold(rendered)(cap => s"$rendered ${renderCapture(cap)}")
    case WildcardPattern(capture) =>
      capture.fold("_")(cap => s"_ ${renderCapture(cap)}")
    case MultiPattern(patterns) =>
      patterns.map(renderPattern).mkString("\n")
    case AlternationPattern(patterns, capture) =>
      val rendered = s"[${patterns.map(renderPattern).mkString(" ")}]"
      capture.fold(rendered)(cap => s"$rendered ${renderCapture(cap)}")

  private def renderFieldPattern(fp: FieldPattern): String =
    s"${FieldName.unwrap(fp.name)}: ${renderPattern(fp.pattern)}"

  private def renderQuantifier(quantifier: Option[QuantifierKind]): String = quantifier match
    case Some(QuantifierKind.Optional)   => "?"
    case Some(QuantifierKind.ZeroOrMore) => "*"
    case Some(QuantifierKind.OneOrMore)  => "+"
    case None                            => ""

  private def renderPredicate(predicate: Predicate): String = predicate match
    case EqPredicate(left, right) =>
      s"(#eq? ${renderPredicateArg(left)} ${renderPredicateArg(right)})"
    case MatchPredicate(arg, regex) =>
      s"""(#match? ${renderPredicateArg(arg)} "${renderString(regex.source)}")"""
    case NotEqPredicate(left, right) =>
      s"(#not-eq? ${renderPredicateArg(left)} ${renderPredicateArg(right)})"
    case NotMatchPredicate(arg, regex) =>
      s"""(#not-match? ${renderPredicateArg(arg)} "${renderString(regex.source)}")"""

  private def renderPredicateArg(arg: PredicateArg): String = arg match
    case CaptureRef(name) => renderCapture(name)
    case StringArg(value) => s""""${renderString(value)}""""

  private def renderCapture(name: CaptureName): String =
    s"@${CaptureName.unwrap(name)}"

  // Note: string content in regex patterns and string literals is used as-is.
  // Escaping is handled at parse time by QueryParser, which stores unescaped values
  // in the AST, so no re-escaping is needed when rendering back to query text.
  // This function exists as an extension point in case escaping is ever required.
  private def renderString(raw: String): String =
    raw
