package morphir.langkit.trees

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.trees.query.*

class QueryParserSpec extends Test[Any]:

  private val leafType: NodeTypeName  = NodeTypeName.make("Leaf").toOption.get
  private val namedType: NodeTypeName = NodeTypeName.make("Named").toOption.get

  private val n: CaptureName     = CaptureName.make("n").toOption.get
  private val b: CaptureName     = CaptureName.make("b").toOption.get
  private val l: CaptureName     = CaptureName.make("l").toOption.get
  private val x: CaptureName     = CaptureName.make("x").toOption.get
  private val outer: CaptureName = CaptureName.make("outer").toOption.get

  private val nameField: FieldName = FieldName.make("name").toOption.get
  private val bodyField: FieldName = FieldName.make("body").toOption.get

  private def rx(s: String): RegexPattern = RegexPattern.make(s).toOption.get

  private def parseOrFail(source: String): Query =
    QueryParser.parse(source) match
      case Success(q)   => q
      case Failure(msg) => throw new AssertionError(s"parse failed: $msg\nSource:\n$source")

  "QueryParser" - {
    "node patterns" - {
      "bare node" in
        assert(parseOrFail("(Leaf)") == Query(NodePattern(leafType, Nil, Nil, None), Nil))
      "node with capture" in
        assert(parseOrFail("(Leaf) @l") == Query(NodePattern(leafType, Nil, Nil, Some(l)), Nil))
      "node with one field" in {
        val expected = Query(
          NodePattern(
            namedType,
            List(FieldPattern(nameField, NodePattern(leafType, Nil, Nil, None))),
            Nil,
            None
          ),
          Nil
        )
        assert(parseOrFail("(Named name: (Leaf))") == expected)
      }
      "node with multiple fields" in {
        val expected = Query(
          NodePattern(
            namedType,
            List(
              FieldPattern(nameField, NodePattern(leafType, Nil, Nil, Some(n))),
              FieldPattern(bodyField, NodePattern(leafType, Nil, Nil, Some(b)))
            ),
            Nil,
            Some(outer)
          ),
          Nil
        )
        assert(parseOrFail("(Named name: (Leaf) @n body: (Leaf) @b) @outer") == expected)
      }
      "node with unfielded child patterns" in {
        val expected = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, Some(n)), NodePattern(leafType, Nil, Nil, Some(b))),
            None
          ),
          Nil
        )
        assert(parseOrFail("(Named (Leaf) @n (Leaf) @b)") == expected)
      }
      "node with mixed field and unfielded child patterns" in {
        val expected = Query(
          NodePattern(
            namedType,
            List(FieldPattern(nameField, NodePattern(leafType, Nil, Nil, Some(n)))),
            List(NodePattern(leafType, Nil, Nil, Some(b))),
            None
          ),
          Nil
        )
        assert(parseOrFail("(Named name: (Leaf) @n (Leaf) @b)") == expected)
      }
      "node with anchored adjacent child patterns" in {
        val expected = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, Some(n)), NodePattern(leafType, Nil, Nil, Some(b))),
            None,
            Set(0)
          ),
          Nil
        )
        assert(parseOrFail("(Named (Leaf) @n . (Leaf) @b)") == expected)
      }
      "node with negated field constraint" in {
        val expected = Query(
          NodePattern(namedType, Nil, Nil, None, Set.empty, Set(nameField)),
          Nil
        )
        assert(parseOrFail("(Named !name)") == expected)
      }
      "alternation pattern parses with capture" in {
        val expected = Query(
          AlternationPattern(
            List(
              NodePattern(leafType, Nil, Nil, Some(n)),
              NodePattern(namedType, Nil, Nil, Some(b))
            ),
            Some(x)
          ),
          Nil
        )
        assert(parseOrFail("[(Leaf) @n (Named) @b] @x") == expected)
      }
      "node with child quantifiers parses" in {
        val expected = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, Some(n)), NodePattern(leafType, Nil, Nil, Some(b))),
            None,
            Set.empty,
            Set.empty,
            Map(0 -> QuantifierKind.Optional, 1 -> QuantifierKind.OneOrMore)
          ),
          Nil
        )
        assert(parseOrFail("(Named (Leaf) @n? (Leaf) @b+)") == expected)
      }
      "field can contain alternation sub-pattern" in {
        val expected = Query(
          NodePattern(
            namedType,
            List(
              FieldPattern(
                nameField,
                AlternationPattern(
                  List(NodePattern(leafType, Nil, Nil, None), WildcardPattern(None)),
                  None
                )
              )
            ),
            Nil,
            None
          ),
          Nil
        )
        assert(parseOrFail("(Named name: [(Leaf) _])") == expected)
      }
      "multiple top-level patterns in one query are accepted" in {
        val res = QueryParser.parse("(Leaf) (Named)")
        assert(res.isSuccess)
      }
      "multiple top-level patterns can still include predicates" in {
        val res = QueryParser.parse("(Leaf) @l (Named) (#eq? @l \"hi\")")
        assert(res.isSuccess)
      }
      "multiple top-level pattern order is deterministic" in {
        val q        = parseOrFail("(Leaf) @l (Named) @n")
        val expected = Query(
          MultiPattern(
            List(
              NodePattern(leafType, Nil, Nil, Some(l)),
              NodePattern(namedType, Nil, Nil, Some(n))
            )
          ),
          Nil
        )
        assert(q == expected)
      }
    }
    "wildcards" - {
      "bare wildcard" in
        assert(parseOrFail("_") == Query(WildcardPattern(None), Nil))
      "parenthesised wildcard with capture" in
        assert(parseOrFail("(_) @x") == Query(WildcardPattern(Some(x)), Nil))
      "wildcard as field sub-pattern" in {
        val expected = Query(
          NodePattern(namedType, List(FieldPattern(bodyField, WildcardPattern(Some(b)))), Nil, None),
          Nil
        )
        assert(parseOrFail("(Named body: _ @b)") == expected)
      }
    }
    "predicates" - {
      "eq? with two capture refs" in {
        val q = parseOrFail("(Leaf) @l (#eq? @l @l)")
        assert(q.predicates == List(EqPredicate(CaptureRef(l), CaptureRef(l))))
      }
      "eq? accepts a string literal on the right" in {
        val q = parseOrFail("(Leaf) @l (#eq? @l \"hello\")")
        assert(q.predicates == List(EqPredicate(CaptureRef(l), StringArg("hello"))))
      }
      "match? pairs capture ref and regex" in {
        val q = parseOrFail("(Leaf) @l (#match? @l \"^hi\")")
        assert(q.predicates == List(MatchPredicate(CaptureRef(l), rx("^hi"))))
      }
      "not-eq? accepts capture ref and string literal" in {
        val q = parseOrFail("(Leaf) @l (#not-eq? @l \"bye\")")
        assert(q.predicates == List(NotEqPredicate(CaptureRef(l), StringArg("bye"))))
      }
      "not-match? accepts capture ref and regex" in {
        val q = parseOrFail("(Leaf) @l (#not-match? @l \"^z\")")
        assert(q.predicates == List(NotMatchPredicate(CaptureRef(l), rx("^z"))))
      }
      "multiple predicates accumulate in order" in {
        val q = parseOrFail("(Leaf) @l (#eq? @l @l) (#match? @l \"x\")")
        assert(q.predicates.size == 2)
      }
      "#eq? with too few args fails cleanly" in
        assert(QueryParser.parse("(Leaf) @l (#eq? @l)").isFailure)
      "#eq? with too many args fails cleanly" in
        assert(QueryParser.parse("(Leaf) @l (#eq? @l @l @l)").isFailure)
      "#match? with too few args fails cleanly" in
        assert(QueryParser.parse("(Leaf) @l (#match? @l)").isFailure)
      "#match? with too many args fails cleanly" in
        assert(QueryParser.parse("(Leaf) @l (#match? @l \"x\" \"y\")").isFailure)
      "#eq? rejects string left-hand side argument kind" in
        assert(QueryParser.parse("(Leaf) @l (#eq? \"x\" @l)").isFailure)
      "#match? rejects string left-hand side argument kind" in
        assert(QueryParser.parse("(Leaf) @l (#match? \"x\" \"^x\")").isFailure)
    }
    "trivia" - {
      "line comments are ignored" in {
        val q = parseOrFail(";; top level\n(Leaf) ;; trailing\n")
        assert(q == Query(NodePattern(leafType, Nil, Nil, None), Nil))
      }
      "whitespace is flexible" in {
        val q = parseOrFail("  \n  ( Leaf   )  \n  @l  ")
        assert(q == Query(NodePattern(leafType, Nil, Nil, Some(l)), Nil))
      }
    }
    "known-type validation" - {
      "accepts queries whose node types are all in knownTypes" in {
        val res = QueryParser.parse("(Leaf)", Set("Leaf"))
        assert(res.isSuccess)
      }
      "rejects an unknown node type with a helpful message" in {
        val res: parsley.Result[String, Query] = QueryParser.parse("(Unknown)", Set("Leaf"))
        val msg: String                        = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.contains("Unknown"))
      }
      "rejects unknown node types appearing in a later top-level pattern" in {
        val res: parsley.Result[String, Query] = QueryParser.parse("(Leaf) (Unknown)", Set("Leaf"))
        val msg: String                        = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.contains("Unknown"))
      }
    }
    "predicate capture validation" - {
      "rejects predicate references to captures that are never bound" in {
        val res: parsley.Result[String, Query] = QueryParser.parse("(Leaf) @l (#eq? @missing \"x\")")
        val msg: String                        = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.contains("unknown capture"))
        assert(msg.contains("@missing"))
      }
      "accepts predicates that reference bound captures" in {
        val res = QueryParser.parse("(Leaf) @l (#eq? @l \"x\")")
        assert(res.isSuccess)
      }
      "rejects duplicate capture names in a single pattern tree" in {
        val res: parsley.Result[String, Query] = QueryParser.parse("(Branch (Leaf) @x (Leaf) @x)")
        val msg: String                        = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.contains("duplicate capture"))
        assert(msg.contains("@x"))
      }
      "rejects duplicate capture names across multi-pattern roots" in {
        val res: parsley.Result[String, Query] = QueryParser.parse("(Leaf) @x (Named) @x")
        val msg: String                        = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.contains("duplicate capture"))
        assert(msg.contains("@x"))
      }
    }
    "errors" - {
      "empty input fails cleanly" in
        assert(QueryParser.parse("").isFailure)
      "unmatched paren fails cleanly" in
        assert(QueryParser.parse("(Leaf").isFailure)
      "missing colon after field name fails cleanly" in
        assert(QueryParser.parse("(Named name (Leaf))").isFailure)
      "invalid regex in #match? fails cleanly" in
        assert(QueryParser.parse("(Leaf) @l (#match? @l \"[unclosed\")").isFailure)
      "malformed input matrix returns stable parse-failure prefix" in {
        val malformed = List(
          "(Leaf",
          "(Leaf) @",
          "(Leaf) @1bad",
          "(Named name (Leaf))",
          "(Leaf) @l (#eq? @l \"unterminated)",
          "(Leaf) @l (#eq? @l)",
          "(Leaf) @l (#match? @l \"[unterminated\")"
        )

        val messages = malformed.map { source =>
          QueryParser.parse(source).toEither.left.getOrElse("")
        }

        assert(messages.forall(_.startsWith("Query parse failed:")))
      }
      "unterminated string literal fails with stable actionable diagnostics" in {
        val res = QueryParser.parse("(Leaf) @l (#eq? @l \"unterminated)")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.startsWith("Query parse failed:"))
        assert(msg.toLowerCase.contains("unexpected") || msg.toLowerCase.contains("expected"))
      }
      "dangling capture sigil fails with parser failure instead of runtime exception" in {
        val res = QueryParser.parse("(Leaf) @")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.startsWith("Query parse failed:"))
      }
      "unknown predicate fails with predicate token in message" in {
        val res = QueryParser.parse("(Leaf) @l (#foo? @l \"x\")")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.contains("#foo?"))
      }
      "unknown predicate has explicit unknown-predicate wording" in {
        val res = QueryParser.parse("(Leaf) @l (#foo? @l \"x\")")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("unknown predicate"))
      }
      "unsupported directive has explicit unsupported-directive wording" in {
        val res = QueryParser.parse("(Leaf) @l (#set! @l \"name\")")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("unsupported directive"))
        assert(msg.contains("#set!"))
      }
      "anchor at beginning of child sequence fails with explicit diagnostic" in {
        val res = QueryParser.parse("(Named . (Leaf) @n (Leaf) @b)")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("invalid anchor placement"))
      }
      "anchor at end of child sequence fails with explicit diagnostic" in {
        val res = QueryParser.parse("(Named (Leaf) @n .)")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("invalid anchor placement"))
      }
      "conflicting positive and negated field constraints fail with explicit diagnostic" in {
        val res = QueryParser.parse("(Named !name name: (Leaf) @n)")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("conflicting field constraints"))
        assert(msg.contains("name"))
      }
      "empty alternation fails with actionable parse diagnostic" in {
        val res = QueryParser.parse("[]")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("alternation requires at least one branch"))
      }
      "quantifier without preceding child pattern fails with explicit diagnostic" in {
        val res = QueryParser.parse("(Named ? (Leaf) @n)")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("invalid quantifier placement"))
      }
      "stacked quantifiers fail with explicit placement diagnostic" in {
        val res = QueryParser.parse("(Named (Leaf) @n?*)")
        val msg = res.toEither.left.getOrElse("")
        assert(res.isFailure)
        assert(msg.toLowerCase.contains("invalid quantifier placement"))
      }
    }
  }
