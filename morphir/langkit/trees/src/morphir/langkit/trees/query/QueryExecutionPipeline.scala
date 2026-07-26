package morphir.langkit.trees.query

import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.QueryableTree
import kyo.*

object QueryExecutionPipeline:

  final case class Analysis(nodeCount: Int, captureCount: Int, predicateCount: Int) derives CanEqual

  final case class Plan(query: Query, analysis: Analysis) derives CanEqual

  final case class Lowered(query: Query, registry: PredicateRegistry) derives CanEqual

  final case class PipelineResult[T](
      normalized: Query,
      analysis: Analysis,
      plan: Plan,
      lowered: Lowered,
      matches: Vector[Match[T]]
  )

  def normalize[Ctx, Err](query: Query)(using Tag[Ctx], Tag[Err]): QueryLogic.QueryEffect[Ctx, String, Err, Query] =
    QueryLogic.log[Ctx, String, Err]("normalize").map(_ => query)

  def analyze[Ctx, Err](query: Query)(using Tag[Ctx], Tag[Err]): QueryLogic.QueryEffect[Ctx, String, Err, Analysis] =
    QueryLogic.log[Ctx, String, Err]("analyze").map { _ =>
      Analysis(
        nodeCount = QueryVisitor.count(query),
        captureCount = query.captureNames.size,
        predicateCount = query.predicates.size
      )
    }

  def validate[Ctx](query: Query)(using Tag[Ctx]): QueryLogic.QueryEffect[Ctx, String, String, Query] =
    QueryLogic.log[Ctx, String, String]("validate").flatMap { _ =>
      val missing = predicateCaptureRefs(query.predicates).diff(query.captureNames)
      if missing.isEmpty then query
      else
        val rendered = missing.toList.map(CaptureName.unwrap).sorted.map(n => s"@$n").mkString(", ")
        QueryLogic
          .error[Ctx, String, String](s"Predicate references unknown capture(s): $rendered")
          .map(_ => query)
    }

  def lower[Ctx, Err](
      query: Query,
      registry: PredicateRegistry = PredicateRegistry.default
  )(using Tag[Ctx], Tag[Err]): QueryLogic.QueryEffect[Ctx, String, Err, Lowered] =
    QueryLogic.log[Ctx, String, Err]("lower").map(_ => Lowered(query, registry))

  def execute[Ctx, T, Err](lowered: Lowered, root: T)(using
      QueryableTree[T],
      Tag[Ctx],
      Tag[Err]
  ): QueryLogic.QueryEffect[Ctx, String, Err, Vector[Match[T]]] =
    QueryLogic.log[Ctx, String, Err]("execute").map { _ =>
      Matcher.matches(lowered.query, root, lowered.registry).toVector
    }

  def run[Ctx, T](
      query: Query,
      root: T,
      initialContext: Ctx,
      registry: PredicateRegistry = PredicateRegistry.default
  )(using QueryableTree[T], Tag[Ctx]): QueryLogic.Result[Ctx, String, String, PipelineResult[T]] =
    QueryLogic.run[Ctx, String, String, PipelineResult[T]](initialContext) {
      for
        normalized <- normalize[Ctx, String](query)
        analysis   <- analyze[Ctx, String](normalized)
        validated  <- validate[Ctx](normalized)
        plan = Plan(validated, analysis)
        lowered <- lower[Ctx, String](plan.query, registry)
        matches <- execute[Ctx, T, String](lowered, root)
      yield PipelineResult(normalized, analysis, plan, lowered, matches)
    }

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

  private def argCapture(arg: PredicateArg): Set[CaptureName] = arg match
    case CaptureRef(name) => Set(name)
    case StringArg(_)     => Set.empty
