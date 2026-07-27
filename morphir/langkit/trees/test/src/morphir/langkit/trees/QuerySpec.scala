package morphir.langkit.trees

import kyo.test.*

import morphir.langkit.trees.query.*

class QuerySpec extends Test[Any]:

  private val leafType: NodeTypeName  = NodeTypeName.make("Leaf").toOption.get
  private val namedType: NodeTypeName = NodeTypeName.make("Named").toOption.get

  private val n: CaptureName     = CaptureName.make("n").toOption.get
  private val b: CaptureName     = CaptureName.make("b").toOption.get
  private val l: CaptureName     = CaptureName.make("l").toOption.get
  private val outer: CaptureName = CaptureName.make("outer").toOption.get
  private val a: CaptureName     = CaptureName.make("a").toOption.get
  private val x: CaptureName     = CaptureName.make("x").toOption.get
  private val kCap: CaptureName  = CaptureName.make("k").toOption.get

  private val nameField: FieldName = FieldName.make("name").toOption.get
  private val bodyField: FieldName = FieldName.make("body").toOption.get

  private def rx(s: String): RegexPattern = RegexPattern.make(s).toOption.get

  "Query AST" - {
    "Pattern" - {
      "NodePattern exposes nodeType, fields, capture" in {
        val p = NodePattern(leafType, Nil, Nil, Some(l))
        assert(p.nodeType == leafType)
        assert(p.fieldPatterns == Nil)
        assert(p.childPatterns == Nil)
        assert(p.capture.contains(l))
      }
      "WildcardPattern captures optional name" in {
        val p: Pattern = WildcardPattern(Some(x))
        assert(p.capture.contains(x))
      }
      "FieldPattern pairs name and sub-pattern" in {
        val inner = NodePattern(leafType, Nil, Nil, None)
        val fp    = FieldPattern(nameField, inner)
        assert(fp.name == nameField)
        assert(fp.pattern == inner)
      }
      "patterns compare structurally" in {
        val pa = NodePattern(namedType, List(FieldPattern(nameField, WildcardPattern(None))), Nil, None)
        val pb = NodePattern(namedType, List(FieldPattern(nameField, WildcardPattern(None))), Nil, None)
        assert(pa == pb)
      }
    }
    "Predicate" - {
      "EqPredicate compares two capture refs" in {
        val p: Predicate = EqPredicate(CaptureRef(a), CaptureRef(b))
        assert(p == EqPredicate(CaptureRef(a), CaptureRef(b)))
      }
      "MatchPredicate pairs capture ref with regex" in {
        val p = MatchPredicate(CaptureRef(x), rx("^foo"))
        assert(p.arg == CaptureRef(x))
        assert(p.regex == rx("^foo"))
      }
      "PredicateArg has CaptureRef and StringArg forms" in {
        val c: PredicateArg = CaptureRef(n)
        val s: PredicateArg = StringArg("literal")
        assert(c != s)
      }
    }
    "Query" - {
      "pairs a root pattern with predicates" in {
        val q = Query(NodePattern(leafType, Nil, Nil, Some(l)), List(MatchPredicate(CaptureRef(l), rx("^hi"))))
        assert(q.root == NodePattern(leafType, Nil, Nil, Some(l)))
        assert(q.predicates.size == 1)
      }
      "captureNames collects every bound name in patterns" in {
        val q = Query(
          NodePattern(
            namedType,
            List(
              FieldPattern(nameField, NodePattern(leafType, Nil, Nil, Some(n))),
              FieldPattern(bodyField, WildcardPattern(Some(b)))
            ),
            Nil,
            Some(outer)
          ),
          Nil
        )
        assert(q.captureNames == Set(outer, n, b))
      }
      "captureNames includes names only referenced by predicates when they also exist on patterns" in {
        val q = Query(
          NodePattern(leafType, Nil, Nil, Some(l)),
          List(EqPredicate(CaptureRef(l), StringArg("x")))
        )
        assert(q.captureNames == Set(l))
      }
      "pretty-print renders canonical query syntax" in {
        val query = Query(
          NodePattern(
            namedType,
            List(FieldPattern(nameField, NodePattern(leafType, Nil, Nil, Some(n)))),
            List(WildcardPattern(Some(b))),
            Some(outer),
            adjacentChildAnchors = Set.empty,
            negatedFields = Set(bodyField),
            childQuantifiers = Map(0 -> QuantifierKind.ZeroOrMore)
          ),
          List(
            NotEqPredicate(CaptureRef(n), StringArg("main")),
            NotMatchPredicate(CaptureRef(n), rx("^tmp"))
          )
        )
        val expected =
          """(Named name: (Leaf) @n !body _ @b*) @outer
            |(#not-eq? @n "main")
            |(#not-match? @n "^tmp")""".stripMargin
        assert(QueryPretty.render(query) == expected)
      }
      "parse -> pretty -> parse roundtrips every supported construct" in {
        val source =
          """(Named name: [(Leaf) @n (Named) @b] @a !body (Leaf) @x . _ @w*)
            |(#eq? @n "x")
            |(#not-eq? @b @x)
            |(#match? @x "^foo")
            |(#not-match? @n "bar$")""".stripMargin
        val parsed   = QueryParser.parse(source).toEither
        val reparsed = parsed.flatMap(q => QueryParser.parse(QueryPretty.render(q)).toEither)
        assert(reparsed == parsed)
      }
      "whitespace-only query differences normalize to same canonical output" in {
        val a        = "(Named name: (Leaf) @n)"
        val b        = "( Named   name:   (Leaf)   @n   )"
        val rendered =
          for
            qa <- QueryParser.parse(a).toEither
            qb <- QueryParser.parse(b).toEither
          yield (QueryPretty.render(qa), QueryPretty.render(qb))
        assert(rendered == Right(("(Named name: (Leaf) @n)", "(Named name: (Leaf) @n)")))
      }
    }
    "Match" - {
      "carries root and capture map" in {
        val m = Match[String]("root", Map(a -> "alpha", b -> "beta"))
        assert(m.root == "root")
        assert(m.captures.size == 2)
        assert(m.captures(a) == "alpha")
      }
      "two matches with identical data compare equal" in {
        val ma = Match("r", Map(kCap -> "v"))
        val mb = Match("r", Map(kCap -> "v"))
        assert(ma == mb)
      }
    }
  }
