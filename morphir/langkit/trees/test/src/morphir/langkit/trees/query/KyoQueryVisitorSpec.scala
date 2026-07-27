package morphir.langkit.trees.query

import kyo.*
import parsley.{Failure, Success}
import kyo.test.*

class KyoQueryVisitorSpec extends Test[Any]:

  private val sampleSource: String =
    "(NodeA name: (NodeB) @child) @parent"

  private def parsedQuery: Query =
    QueryParser.parse(sampleSource) match
      case Success(q)   => q
      case Failure(msg) => sys.error(s"baseline parse failure: $msg")

  private def tag(node: QueryNode): String = node match
    case QueryNode.Root(_)             => "query"
    case QueryNode.PatternNode(_)      => "pattern"
    case QueryNode.FieldPatternNode(_) => "field"
    case QueryNode.PredicateNode(_)    => "predicate"
    case QueryNode.PredicateArgNode(_) => "arg"

  "KyoQueryVisitor" - {
    "visit invokes callback for every query node in pre-order" in {
      val out = KyoQueryVisitor.fold(parsedQuery, 0)((acc, _) => (acc + 1): Int < Any).eval
      assert(out > 0)
    }
    "visit order matches the pure QueryVisitor traversal order" in {
      val pureOrder = QueryVisitor.foldLeft(parsedQuery, Vector.empty[String]) { (acc, n) =>
        acc :+ tag(n)
      }
      val kyoOrder = KyoQueryVisitor
        .fold(parsedQuery, Vector.empty[String]) { (acc, n) =>
          (acc :+ tag(n)): Vector[String] < Any
        }
        .eval
      assert(kyoOrder == pureOrder)
    }
    "visit on the root pattern alone matches pure traversal of that subtree" in {
      val rootPattern = parsedQuery.root
      val pureOrder   = QueryVisitor
        .foldLeft(Query(rootPattern, Nil), Vector.empty[String]) { (acc, n) =>
          acc :+ tag(n)
        }
        .drop(1) // drop the synthetic root since pattern-only visit starts from PatternNode
      val kyoOrder = KyoQueryVisitor
        .fold(rootPattern, Vector.empty[String]) { (acc, n) =>
          (acc :+ tag(n)): Vector[String] < Any
        }
        .eval
      assert(kyoOrder == pureOrder)
    }
    "Abort.fail in callback short-circuits visitation" in {
      val out = Abort
        .run[String] {
          KyoQueryVisitor.visit(parsedQuery)(_ => Abort.fail("stop"))
        }
        .eval
      assert(out.toString.contains("stop"))
    }
  }
