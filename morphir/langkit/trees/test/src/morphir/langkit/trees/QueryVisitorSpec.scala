package morphir.langkit.trees

import kyo.test.*

import morphir.langkit.trees.query.*

class QueryVisitorSpec extends Test[Any]:

  private val namedType: NodeTypeName = NodeTypeName.make("Named").toOption.get
  private val leafType: NodeTypeName  = NodeTypeName.make("Leaf").toOption.get
  private val n: CaptureName          = CaptureName.make("n").toOption.get
  private val b: CaptureName          = CaptureName.make("b").toOption.get
  private val nameField: FieldName    = FieldName.make("name").toOption.get

  private val sampleQuery: Query =
    Query(
      NodePattern(
        namedType,
        List(FieldPattern(nameField, NodePattern(leafType, Nil, Nil, Some(n)))),
        List(WildcardPattern(Some(b))),
        None,
        childQuantifiers = Map(0 -> QuantifierKind.Optional)
      ),
      List(EqPredicate(CaptureRef(n), StringArg("main")))
    )

  private def tag(node: QueryNode): String = node match
    case QueryNode.Root(_)             => "query"
    case QueryNode.PatternNode(_)      => "pattern"
    case QueryNode.FieldPatternNode(_) => "field"
    case QueryNode.PredicateNode(_)    => "predicate"
    case QueryNode.PredicateArgNode(_) => "arg"

  "QueryVisitor" - {
    "children of root includes root pattern then predicates in order" in {
      val kinds = QueryVisitor.children(QueryNode.Root(sampleQuery)).map(tag)
      assert(kinds == List("pattern", "predicate"))
    }
    "foldLeft is deterministic pre-order over query nodes" in {
      val seen = QueryVisitor.foldLeft(sampleQuery, Vector.empty[String]) { (acc, node) =>
        acc :+ tag(node)
      }
      assert(seen == Vector("query", "pattern", "field", "pattern", "pattern", "predicate", "arg", "arg"))
    }
    "collect can extract captures from pattern and predicate args" in {
      val captures = QueryVisitor.collect(sampleQuery) {
        case QueryNode.PatternNode(NodePattern(_, _, _, Some(capture), _, _, _)) => CaptureName.unwrap(capture)
        case QueryNode.PatternNode(WildcardPattern(Some(capture)))               => CaptureName.unwrap(capture)
        case QueryNode.PredicateArgNode(CaptureRef(name))                        => CaptureName.unwrap(name)
      }
      assert(captures == List("n", "b", "n"))
    }
    "traverse executes effects in pre-order with context and logs" in {
      val result = QueryLogic.run[Int, String, String, Int](initialContext = 0) {
        for
          _ <- QueryVisitor.traverse(sampleQuery) { node =>
            for
              _ <- QueryLogic.log[Int, String, String](tag(node))
              _ <- QueryLogic.updateContext[Int, String, String](_ + 1)
            yield ()
          }
          ctx <- QueryLogic.readContext[Int, String, String]
        yield ctx
      }
      assert(result.value == Right(8))
      assert(result.context == 8)
      assert(result.logs == Vector("query", "pattern", "field", "pattern", "pattern", "predicate", "arg", "arg"))
    }
  }
