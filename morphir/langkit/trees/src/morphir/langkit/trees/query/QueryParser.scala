package morphir.langkit.trees.query

import parsley.Parsley
import parsley.Parsley.{atomic, eof, many}
import parsley.Result
import parsley.character.{char, digit, letter, satisfy, stringOfMany}
import parsley.combinator.option
import parsley.errors.combinator.ErrorMethods
import parsley.syntax.character.{charLift, stringLift}

import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName

/**
 * Parses tree-sitter-style query strings into the v1 query AST.
 *
 * Supported v1 syntax:
 *   - Node pattern: `(NodeType)`
 *   - Field pattern: `(NodeType field_name: (ChildType))`
 *   - Capture: `@name` appended to any pattern
 *   - Wildcard: `_` or `(_)`, optionally followed by `@name`
 *   - Predicate clauses at top level: `(#eq? @a @b)`, `(#not-eq? @a "x")`, `(#match? @x "regex")`,
 *     `(#not-match? @x "regex")`
 *   - Line comments: `;; ...` to end of line
 *   - Flexible whitespace between tokens
 *
 * Quantifiers are deferred to the v2 epic.
 */
object QueryParser:

  private val parseFailurePrefix = "Query parse failed: "

  /** Parse a query without validating node-type names. */
  def parse(source: String): Result[String, Query] =
    queryParser.parse(source) match
      case parsley.Success(q) =>
        val dups = duplicateCaptureNames(q.root)
        if dups.nonEmpty then
          val rendered = dups.toList.map(CaptureName.unwrap).sorted.map(n => s"@$n").mkString(", ")
          parsley.Failure(s"duplicate capture name(s): $rendered")
        else
          val missing = predicateCaptureRefs(q.predicates).diff(captureNames(q.root))
          if missing.isEmpty then parsley.Success(q)
          else
            val rendered = missing.toList.map(CaptureName.unwrap).sorted.map(n => s"@$n").mkString(", ")
            parsley.Failure(s"Predicate references unknown capture(s): $rendered")
      case parsley.Failure(msg) =>
        parsley.Failure(normalizeFailure(msg.toString))

  /**
   * Parse a query and validate every node-type name against `knownTypes`.
   *
   * Unknown names produce a Failure with the offending identifiers listed alphabetically.
   */
  def parse(source: String, knownTypes: Set[String]): Result[String, Query] =
    parse(source) match
      case parsley.Success(q) =>
        val unknown = gatherNodeTypes(q.root).map(NodeTypeName.unwrap).diff(knownTypes)
        if unknown.isEmpty then parsley.Success(q)
        else parsley.Failure(s"Unknown node type(s): ${unknown.toList.sorted.mkString(", ")}")
      case f: parsley.Failure[String] => f

  private def gatherNodeTypes(p: Pattern): Set[NodeTypeName] = p match
    case NodePattern(nt, fields, children, _, _, _, _) =>
      val withFields = fields.foldLeft(Set(nt))((acc, fp) => acc ++ gatherNodeTypes(fp.pattern))
      children.foldLeft(withFields)((acc, child) => acc ++ gatherNodeTypes(child))
    case MultiPattern(patterns) =>
      patterns.foldLeft(Set.empty[NodeTypeName])((acc, child) => acc ++ gatherNodeTypes(child))
    case AlternationPattern(patterns, _) =>
      patterns.foldLeft(Set.empty[NodeTypeName])((acc, child) => acc ++ gatherNodeTypes(child))
    case _: WildcardPattern => Set.empty

  private def captureNames(p: Pattern): Set[CaptureName] = p match
    case NodePattern(_, fields, children, capture, _, _, _) =>
      val own        = capture.toSet
      val fromFields = fields.foldLeft(Set.empty[CaptureName])((acc, fp) => acc ++ captureNames(fp.pattern))
      val fromKids   = children.foldLeft(Set.empty[CaptureName])((acc, kid) => acc ++ captureNames(kid))
      own ++ fromFields ++ fromKids
    case MultiPattern(patterns) =>
      patterns.foldLeft(Set.empty[CaptureName])((acc, pattern) => acc ++ captureNames(pattern))
    case AlternationPattern(patterns, capture) =>
      capture.toSet ++ patterns.foldLeft(Set.empty[CaptureName])((acc, pattern) => acc ++ captureNames(pattern))
    case WildcardPattern(capture) =>
      capture.toSet

  private def predicateCaptureRefs(predicates: List[Predicate]): Set[CaptureName] =
    predicates.foldLeft(Set.empty[CaptureName]) { (acc, p) =>
      p match
        case EqPredicate(left, right) =>
          acc ++ argCapture(left) ++ argCapture(right)
        case MatchPredicate(arg, _) =>
          acc ++ argCapture(arg)
        case NotEqPredicate(left, right) =>
          acc ++ argCapture(left) ++ argCapture(right)
        case NotMatchPredicate(arg, _) =>
          acc ++ argCapture(arg)
    }

  private def duplicateCaptureNames(p: Pattern): Set[CaptureName] =
    captureNameCounts(p).collect { case (name, count) if count > 1 => name }.toSet

  private def captureNameCounts(p: Pattern): Map[CaptureName, Int] =
    val own: Map[CaptureName, Int] = p.capture match
      case Some(name) => Map(name -> 1)
      case None       => Map.empty

    p match
      case NodePattern(_, fields, children, _, _, _, _) =>
        val fromFields = fields.map(fp => captureNameCounts(fp.pattern))
        val fromKids   = children.map(captureNameCounts)
        (own :: (fromFields ::: fromKids)).foldLeft(Map.empty[CaptureName, Int]) { (acc, m) =>
          m.foldLeft(acc) { case (acc2, (name, count)) =>
            acc2.updated(name, acc2.getOrElse(name, 0) + count)
          }
        }
      case MultiPattern(patterns) =>
        (own :: patterns.map(captureNameCounts)).foldLeft(Map.empty[CaptureName, Int]) { (acc, m) =>
          m.foldLeft(acc) { case (acc2, (name, count)) =>
            acc2.updated(name, acc2.getOrElse(name, 0) + count)
          }
        }
      case AlternationPattern(patterns, _) =>
        (own :: patterns.map(captureNameCounts)).foldLeft(Map.empty[CaptureName, Int]) { (acc, m) =>
          m.foldLeft(acc) { case (acc2, (name, count)) =>
            acc2.updated(name, acc2.getOrElse(name, 0) + count)
          }
        }
      case _: WildcardPattern =>
        own

  private def argCapture(arg: PredicateArg): Option[CaptureName] = arg match
    case CaptureRef(name) => Some(name)
    case StringArg(_)     => None

  private def normalizeFailure(msg: String): String =
    val withPrefix                  = s"$parseFailurePrefix$msg"
    val unknownPredicatePattern     = """unexpected \"(#[\w-]+\?)\"""".r
    val unsupportedDirectivePattern = """unexpected \"(#[\w-]+!)\"""".r
    unsupportedDirectivePattern.findFirstMatchIn(msg) match
      case Some(m) if msg.contains("expected") =>
        val token = m.group(1)
        s"$parseFailurePrefix Unsupported directive: $token\n$msg"
      case _ =>
        unknownPredicatePattern.findFirstMatchIn(msg) match
          case Some(m) if msg.contains("expected") =>
            val token = m.group(1)
            s"$parseFailurePrefix Unknown predicate: $token\n$msg"
          case _ =>
            withPrefix

  // --- Trivia --------------------------------------------------------------

  private val lineComment: Parsley[Unit] =
    atomic(";;") *> many(satisfy(_ != '\n')).as(())

  private val whitespaceChar: Parsley[Unit] =
    satisfy(_.isWhitespace).as(())

  private val skipTrivia: Parsley[Unit] =
    many(whitespaceChar | lineComment).as(())

  private def tok[A](p: Parsley[A]): Parsley[A] = p <* skipTrivia

  // --- Lexemes -------------------------------------------------------------

  private val identStart: Parsley[Char] = letter | char('_')
  private val identCont: Parsley[Char]  = letter | digit | char('_')

  /**
   * Raw identifier text. The grammar guarantees non-empty, non-whitespace, starting with a letter or underscore —
   * exactly what [[NodeTypeName]], [[FieldName]], and [[CaptureName]] require — so the `unsafeMake` wrappers below are
   * safe by construction.
   */
  private val identifier: Parsley[String] = (identStart <~> stringOfMany(identCont)).map { case (h, t) => s"$h$t" }

  private val nodeTypeName: Parsley[NodeTypeName] = identifier.map(NodeTypeName.unsafeMake)
  private val fieldName: Parsley[FieldName]       = identifier.map(FieldName.unsafeMake)
  private val captureName: Parsley[CaptureName]   = identifier.map(CaptureName.unsafeMake)

  private val stringLit: Parsley[String] =
    char('"') *> stringOfMany(satisfy(_ != '"')) <* char('"')

  private val captureTail: Parsley[CaptureName] = char('@') *> captureName

  // --- Patterns ------------------------------------------------------------

  // Lazy forward reference so nested patterns work.
  private lazy val pattern: Parsley[Pattern] =
    alternationPattern | wildcardPattern | nodePattern

  private lazy val wildcardPattern: Parsley[Pattern] =
    val parenthesised = atomic(tok('(' <~ skipTrivia) *> tok(char('_')) <* tok(')'))
    val bare          = tok(char('_'))
    (parenthesised | bare) *> option(tok(captureTail)).map(WildcardPattern(_))

  private lazy val alternationPattern: Parsley[Pattern] =
    (
      tok('[' <~ skipTrivia)
        *> many(pattern)
        <~ tok(']')
        <~> option(tok(captureTail))
    ).flatMap { case (branches, cap) =>
      if branches.isEmpty then Parsley.empty.label("alternation requires at least one branch")
      else Parsley.pure(AlternationPattern(branches, cap))
    }

  private lazy val nodePattern: Parsley[Pattern] =
    (
      tok('(' <~ skipTrivia)
        *> tok(nodeTypeName)
        <~> many(nodeMember)
        <~ tok(')')
        <~> option(tok(captureTail))
    ).flatMap { case ((nt, members), cap) =>
      buildNodePattern(nt, members, cap) match
        case Right(p)  => Parsley.pure(p)
        case Left(msg) => Parsley.empty.label(msg)
    }

  private enum NodeMember derives CanEqual:
    case Field(value: FieldPattern)
    case Child(value: Pattern)
    case Anchor
    case NegatedField(value: FieldName)
    case Quantifier(value: QuantifierKind)

  private lazy val nodeMember: Parsley[NodeMember] =
    fieldPattern.map(NodeMember.Field(_)) |
      (tok(char('!')) *> tok(fieldName)).map(NodeMember.NegatedField(_)) |
      tok(char('.')).as(NodeMember.Anchor) |
      tok(char('?')).as(NodeMember.Quantifier(QuantifierKind.Optional)) |
      tok(char('*')).as(NodeMember.Quantifier(QuantifierKind.ZeroOrMore)) |
      tok(char('+')).as(NodeMember.Quantifier(QuantifierKind.OneOrMore)) |
      pattern.map(NodeMember.Child(_))

  private lazy val fieldPattern: Parsley[FieldPattern] = (atomic(tok(fieldName) <~ tok(':')) <~> pattern).map {
    case (name, p) =>
      FieldPattern(name, p)
  }

  private def buildNodePattern(
      nt: NodeTypeName,
      members: List[NodeMember],
      cap: Option[CaptureName]
  ): Either[String, NodePattern] =
    val invalidAnchorIndex = members.indices.find { i =>
      members(i) == NodeMember.Anchor && (
        i == 0 ||
          i == members.size - 1 ||
          !members(i - 1).isInstanceOf[NodeMember.Child] ||
          !members(i + 1).isInstanceOf[NodeMember.Child]
      )
    }

    invalidAnchorIndex match
      case Some(_) =>
        Left("invalid anchor placement: '.' must appear between two unfielded child patterns")
      case None =>
        val invalidQuantifierIndex = members.indices.find { i =>
          members(i) match
            case NodeMember.Quantifier(_) =>
              i == 0 || !members(i - 1).isInstanceOf[NodeMember.Child]
            case _ => false
        }
        invalidQuantifierIndex match
          case Some(_) =>
            Left("invalid quantifier placement: quantifiers must follow an unfielded child pattern")
          case None =>
            val fields            = members.collect { case NodeMember.Field(value) => value }
            val children          = members.collect { case NodeMember.Child(value) => value }
            val negatedFields     = members.collect { case NodeMember.NegatedField(value) => value }.toSet
            val conflictingFields = fields.map(_.name).toSet.intersect(negatedFields)
            if conflictingFields.nonEmpty then
              Left(
                s"conflicting field constraints: ${conflictingFields.toList.map(FieldName.unwrap).sorted.mkString(", ")}"
              )
            else
              val anchors = members.indices.collect {
                case i if members(i) == NodeMember.Anchor =>
                  members.take(i).count(_.isInstanceOf[NodeMember.Child]) - 1
              }.toSet
              val quantifiers = members.indices.collect {
                case i if members(i).isInstanceOf[NodeMember.Quantifier] =>
                  val childIdx = members.take(i).count(_.isInstanceOf[NodeMember.Child]) - 1
                  val q        = members(i) match
                    case NodeMember.Quantifier(value) => value
                    case _                            =>
                      throw new IllegalStateException("unreachable quantifier shape")
                  (childIdx, q)
              }.toMap
              Right(NodePattern(nt, fields, children, cap, anchors, negatedFields, quantifiers))

  // --- Predicates ----------------------------------------------------------

  private val captureArg: Parsley[CaptureRef] =
    tok(captureTail).map(CaptureRef(_))

  private val predicateArg: Parsley[PredicateArg] =
    captureArg | tok(stringLit).map(StringArg(_))

  private val eqPredicate: Parsley[Predicate] =
    (atomic(tok('(' <~ skipTrivia) *> tok("#eq?")) *> captureArg <~> predicateArg <~ tok(')'))
      .map { case (l, r) => EqPredicate(l, r) }

  private val regexArg: Parsley[RegexPattern] =
    tok(stringLit).flatMap { raw =>
      RegexPattern.make(raw) match
        case Right(rx) => Parsley.pure(rx)
        case Left(msg) => Parsley.empty.label(msg)
    }

  private val matchPredicate: Parsley[Predicate] =
    (
      atomic(tok('(' <~ skipTrivia) *> tok("#match?"))
        *> captureArg
        <~> regexArg
        <~ tok(')')
    ).map { case (a, r) => MatchPredicate(a, r) }

  private val notEqPredicate: Parsley[Predicate] =
    (atomic(tok('(' <~ skipTrivia) *> tok("#not-eq?")) *> captureArg <~> predicateArg <~ tok(')'))
      .map { case (l, r) => NotEqPredicate(l, r) }

  private val notMatchPredicate: Parsley[Predicate] =
    (
      atomic(tok('(' <~ skipTrivia) *> tok("#not-match?"))
        *> captureArg
        <~> regexArg
        <~ tok(')')
    ).map { case (a, r) => NotMatchPredicate(a, r) }

  private val predicate: Parsley[Predicate] =
    eqPredicate | matchPredicate | notEqPredicate | notMatchPredicate

  // --- Top-level -----------------------------------------------------------

  private val queryPart: Parsley[Either[Pattern, Predicate]] =
    predicate.map(Right(_)) | pattern.map(Left(_))

  private val queryParser: Parsley[Query] = (skipTrivia *> many(queryPart) <~ eof).flatMap { parts =>
    val roots = parts.collect { case Left(p) => p }
    val preds = parts.collect { case Right(pr) => pr }
    roots match
      case Nil =>
        Parsley.empty.label("at least one query pattern")
      case first :: rest =>
        val root = if rest.isEmpty then first else MultiPattern(first :: rest)
        Parsley.pure(Query(root, preds))
  }
