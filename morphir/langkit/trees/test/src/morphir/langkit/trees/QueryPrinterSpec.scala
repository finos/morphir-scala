package morphir.langkit.trees

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.trees.query.*

/** Verifies that [[QueryPrinter]] produces canonical S-expression output that round-trips through [[QueryParser]]. */
class QueryPrinterSpec extends Test[Any]:

  private val leafType: NodeTypeName  = NodeTypeName.make("Leaf").toOption.get
  private val namedType: NodeTypeName = NodeTypeName.make("Named").toOption.get

  private val n: CaptureName     = CaptureName.make("n").toOption.get
  private val b: CaptureName     = CaptureName.make("b").toOption.get
  private val l: CaptureName     = CaptureName.make("l").toOption.get
  private val outer: CaptureName = CaptureName.make("outer").toOption.get
  private val x: CaptureName     = CaptureName.make("x").toOption.get

  private val nameField: FieldName = FieldName.make("name").toOption.get
  private val bodyField: FieldName = FieldName.make("body").toOption.get

  private def rx(s: String): RegexPattern = RegexPattern.make(s).toOption.get

  private def parse(source: String): Query = QueryParser.parse(source) match
    case Success(q)   => q
    case Failure(msg) => throw new AssertionError(s"parse failed: $msg\nSource:\n$source")

  private def roundTrips(source: String): Boolean =
    val canonical = QueryPrinter.print(parse(source))
    QueryParser.parse(canonical).isSuccess

  "QueryPrinter" - {
    "node patterns" - {
      "bare node pattern" in {
        val q   = Query(NodePattern(leafType, Nil, Nil, None), Nil)
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf)")
      }
      "node with capture" in {
        val q   = Query(NodePattern(leafType, Nil, Nil, Some(l)), Nil)
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l")
      }
      "node with field pattern" in {
        val q = Query(
          NodePattern(
            namedType,
            List(FieldPattern(nameField, NodePattern(leafType, Nil, Nil, None))),
            Nil,
            None
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named name: (Leaf))")
      }
      "node with multiple fields and outer capture" in {
        val q = Query(
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
        val out = QueryPrinter.print(q)
        assert(out == "(Named name: (Leaf) @n body: (Leaf) @b) @outer")
      }
      "node with unfielded child patterns" in {
        val q = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, Some(n)), NodePattern(leafType, Nil, Nil, Some(b))),
            None
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named (Leaf) @n (Leaf) @b)")
      }
      "node with anchored adjacent child patterns" in {
        val q = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, Some(n)), NodePattern(leafType, Nil, Nil, Some(b))),
            None,
            Set(0)
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named (Leaf) @n . (Leaf) @b)")
      }
      "node with negated field constraint" in {
        val q = Query(
          NodePattern(namedType, Nil, Nil, None, Set.empty, Set(nameField)),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named !name)")
      }
      "node with optional child quantifier" in {
        val q = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, Some(n))),
            None,
            Set.empty,
            Set.empty,
            Map(0 -> QuantifierKind.Optional)
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named (Leaf) @n?)")
      }
      "node with zero-or-more child quantifier" in {
        val q = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, None)),
            None,
            Set.empty,
            Set.empty,
            Map(0 -> QuantifierKind.ZeroOrMore)
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named (Leaf)*)")
      }
      "node with one-or-more child quantifier" in {
        val q = Query(
          NodePattern(
            namedType,
            Nil,
            List(NodePattern(leafType, Nil, Nil, None)),
            None,
            Set.empty,
            Set.empty,
            Map(0 -> QuantifierKind.OneOrMore)
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Named (Leaf)+)")
      }
    }
    "wildcards" - {
      "bare wildcard" in
        assert(QueryPrinter.print(Query(WildcardPattern(None), Nil)) == "_")
      "wildcard with capture" in
        assert(QueryPrinter.print(Query(WildcardPattern(Some(x)), Nil)) == "_ @x")
    }
    "alternation" - {
      "alternation without capture" in {
        val q = Query(
          AlternationPattern(
            List(NodePattern(leafType, Nil, Nil, Some(n)), NodePattern(namedType, Nil, Nil, Some(b))),
            None
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "[(Leaf) @n (Named) @b]")
      }
      "alternation with outer capture" in {
        val q = Query(
          AlternationPattern(
            List(NodePattern(leafType, Nil, Nil, None), WildcardPattern(None)),
            Some(x)
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "[(Leaf) _] @x")
      }
    }
    "predicates" - {
      "#eq? with two capture refs" in {
        val q   = Query(NodePattern(leafType, Nil, Nil, Some(l)), List(EqPredicate(CaptureRef(l), CaptureRef(n))))
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (#eq? @l @n)")
      }
      "#eq? with string literal on the right" in {
        val q = Query(
          NodePattern(leafType, Nil, Nil, Some(l)),
          List(EqPredicate(CaptureRef(l), StringArg("hello")))
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (#eq? @l \"hello\")")
      }
      "#match? with capture ref and regex" in {
        val q   = Query(NodePattern(leafType, Nil, Nil, Some(l)), List(MatchPredicate(CaptureRef(l), rx("^hi"))))
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (#match? @l \"^hi\")")
      }
      "#not-eq? predicate" in {
        val q = Query(
          NodePattern(leafType, Nil, Nil, Some(l)),
          List(NotEqPredicate(CaptureRef(l), StringArg("bye")))
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (#not-eq? @l \"bye\")")
      }
      "#not-match? predicate" in {
        val q =
          Query(NodePattern(leafType, Nil, Nil, Some(l)), List(NotMatchPredicate(CaptureRef(l), rx("^z"))))
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (#not-match? @l \"^z\")")
      }
      "multiple predicates are appended in order" in {
        val q = Query(
          NodePattern(leafType, Nil, Nil, Some(l)),
          List(EqPredicate(CaptureRef(l), StringArg("x")), MatchPredicate(CaptureRef(l), rx("y")))
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (#eq? @l \"x\") (#match? @l \"y\")")
      }
    }
    "multi-pattern" - {
      "two top-level patterns" in {
        val q = Query(
          MultiPattern(
            List(
              NodePattern(leafType, Nil, Nil, Some(l)),
              NodePattern(namedType, Nil, Nil, Some(n))
            )
          ),
          Nil
        )
        val out = QueryPrinter.print(q)
        assert(out == "(Leaf) @l (Named) @n")
      }
    }
    "round-trip" - {
      "bare node round-trips" in
        assert(roundTrips("(Leaf)"))
      "node with capture round-trips" in
        assert(roundTrips("(Leaf) @l"))
      "node with field round-trips" in
        assert(roundTrips("(Named name: (Leaf) @n)"))
      "node with multiple fields round-trips" in
        assert(roundTrips("(Named name: (Leaf) @n body: (Leaf) @b) @outer"))
      "node with child patterns round-trips" in
        assert(roundTrips("(Named (Leaf) @n (Leaf) @b)"))
      "node with anchor round-trips" in
        assert(roundTrips("(Named (Leaf) @n . (Leaf) @b)"))
      "node with negated field round-trips" in
        assert(roundTrips("(Named !name)"))
      "node with optional quantifier round-trips" in
        assert(roundTrips("(Named (Leaf) @n?)"))
      "wildcard round-trips" in
        assert(roundTrips("_"))
      "wildcard with capture round-trips" in
        assert(roundTrips("(_ ) @x"))
      "alternation round-trips" in
        assert(roundTrips("[(Leaf) @n (Named) @b]"))
      "alternation with capture round-trips" in
        assert(roundTrips("[(Leaf) _] @x"))
      "#eq? predicate round-trips" in
        assert(roundTrips("(Leaf) @l (#eq? @l \"hello\")"))
      "#match? predicate round-trips" in
        assert(roundTrips("(Leaf) @l (#match? @l \"^hi\")"))
      "multi-pattern round-trips" in
        assert(roundTrips("(Leaf) @l (Named) @n"))
      "query with comment is canonical without comment" in {
        val out = QueryPrinter.print(parse(";; comment\n(Leaf) @l"))
        assert(QueryParser.parse(out).isSuccess)
      }
    }
  }
