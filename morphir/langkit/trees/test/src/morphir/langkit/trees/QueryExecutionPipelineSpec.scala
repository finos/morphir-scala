package morphir.langkit.trees

import kyo.test.*

import morphir.langkit.trees.ToyTree.*
import morphir.langkit.trees.query.*

class QueryExecutionPipelineSpec extends Test[Any]:

  private val namedType: NodeTypeName = NodeTypeName.make("Named").toOption.get
  private val leafType: NodeTypeName  = NodeTypeName.make("Leaf").toOption.get
  private val n: CaptureName          = CaptureName.make("n").toOption.get
  private val missing: CaptureName    = CaptureName.make("missing").toOption.get
  private val nameField: FieldName    = FieldName.make("name").toOption.get

  private val tree: ToyTree =
    Named(Leaf("main"), Leaf("42"))

  "QueryExecutionPipeline" - {
    "run executes normalize/analyze/validate/lower/execute with deterministic logs" in {
      val query = Query(
        NodePattern(
          namedType,
          List(FieldPattern(nameField, NodePattern(leafType, Nil, Nil, Some(n)))),
          Nil,
          None
        ),
        Nil
      )
      val run = QueryExecutionPipeline.run[Int, ToyTree](query, tree, initialContext = 1)
      assert(run.value.isRight)
      assert(run.logs == Vector("normalize", "analyze", "validate", "lower", "execute"))
      assert(run.value.toOption.exists(_.analysis.captureCount == 1))
      assert(run.value.toOption.exists(_.matches.nonEmpty))
    }
    "validate accumulates unknown-capture diagnostics as errors" in {
      val query = Query(
        NodePattern(namedType, Nil, Nil, None),
        List(EqPredicate(CaptureRef(missing), StringArg("x")))
      )
      val run = QueryExecutionPipeline.run[Int, ToyTree](query, tree, initialContext = 0)
      assert(run.value.isLeft)
      assert(run.errors.exists(_.contains("@missing")))
    }
  }
