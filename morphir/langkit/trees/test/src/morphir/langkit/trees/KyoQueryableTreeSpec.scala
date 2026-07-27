package morphir.langkit.trees

import kyo.*
import kyo.test.*

class KyoQueryableTreeSpec extends Test[Any]:
  final case class SampleNode(id: Int, kids: List[SampleNode]) derives CanEqual

  private given QueryableTree[SampleNode] with
    def nodeType(t: SampleNode): NodeTypeName                  = NodeTypeName.unsafeMake(s"node-${t.id}")
    def children(t: SampleNode): Seq[SampleNode]               = t.kids
    def text(t: SampleNode): Option[String]                    = None
    def fields(t: SampleNode): Map[FieldName, Seq[SampleNode]] = Map.empty

  private val tree: SampleNode =
    SampleNode(1, List(SampleNode(2, Nil), SampleNode(3, List(SampleNode(4, Nil)))))

  "KyoQueryableTree" - {
    "traverseKyo visits every node in pre-order" in {
      val out = KyoQueryableTree
        .foldKyo(tree, Vector.empty[Int]) { (acc, n) =>
          (acc :+ n.id): Vector[Int] < Any
        }
        .eval
      assert(out == Vector(1, 2, 3, 4))
    }
    "foldKyo accumulates left-to-right pre-order" in {
      val out = KyoQueryableTree
        .foldKyo(tree, 0) { (acc, n) =>
          (acc + n.id): Int < Any
        }
        .eval
      assert(out == 10)
    }
    "traverseKyo on a single-node tree visits exactly once" in {
      val out = KyoQueryableTree
        .foldKyo(SampleNode(99, Nil), 0) { (acc, _) =>
          (acc + 1): Int < Any
        }
        .eval
      assert(out == 1)
    }
    "Abort.fail in callback short-circuits traversal" in {
      val out = Abort
        .run[String] {
          KyoQueryableTree.foldKyo(tree, 0) { (acc, n) =>
            if n.id == 3 then Abort.fail("stop at 3")
            else (acc + n.id): Int < Any
          }
        }
        .eval
      assert(out.toString.contains("stop at 3"))
    }
  }
