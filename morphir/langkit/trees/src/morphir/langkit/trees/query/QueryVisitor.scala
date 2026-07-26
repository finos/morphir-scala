package morphir.langkit.trees.query

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls.*

enum QueryNode derives CanEqual:
  case Root(value: Query)
  case PatternNode(value: Pattern)
  case FieldPatternNode(value: FieldPattern)
  case PredicateNode(value: Predicate)
  case PredicateArgNode(value: PredicateArg)

trait QueryVisitor[A]:

  def visitNode(node: QueryNode): A

  def visitQuery(node: Query): A                           = visitNode(QueryNode.Root(node))
  def visitPattern(node: Pattern): A                       = visitNode(QueryNode.PatternNode(node))
  def visitNodePattern(node: NodePattern): A               = visitPattern(node)
  def visitMultiPattern(node: MultiPattern): A             = visitPattern(node)
  def visitAlternationPattern(node: AlternationPattern): A = visitPattern(node)
  def visitWildcardPattern(node: WildcardPattern): A       = visitPattern(node)
  def visitFieldPattern(node: FieldPattern): A             = visitNode(QueryNode.FieldPatternNode(node))
  def visitPredicate(node: Predicate): A                   = visitNode(QueryNode.PredicateNode(node))
  def visitEqPredicate(node: EqPredicate): A               = visitPredicate(node)
  def visitMatchPredicate(node: MatchPredicate): A         = visitPredicate(node)
  def visitNotEqPredicate(node: NotEqPredicate): A         = visitPredicate(node)
  def visitNotMatchPredicate(node: NotMatchPredicate): A   = visitPredicate(node)
  def visitPredicateArg(node: PredicateArg): A             = visitNode(QueryNode.PredicateArgNode(node))
  def visitCaptureRef(node: CaptureRef): A                 = visitPredicateArg(node)
  def visitStringArg(node: StringArg): A                   = visitPredicateArg(node)

object QueryVisitor:

  def visit[A](node: QueryNode, visitor: QueryVisitor[A]): A = node match
    case QueryNode.Root(query)          => visitor.visitQuery(query)
    case QueryNode.PatternNode(pattern) =>
      pattern match
        case n: NodePattern        => visitor.visitNodePattern(n)
        case m: MultiPattern       => visitor.visitMultiPattern(m)
        case a: AlternationPattern => visitor.visitAlternationPattern(a)
        case w: WildcardPattern    => visitor.visitWildcardPattern(w)
    case QueryNode.FieldPatternNode(fieldPattern) =>
      visitor.visitFieldPattern(fieldPattern)
    case QueryNode.PredicateNode(predicate) =>
      predicate match
        case eq: EqPredicate           => visitor.visitEqPredicate(eq)
        case m: MatchPredicate         => visitor.visitMatchPredicate(m)
        case neq: NotEqPredicate       => visitor.visitNotEqPredicate(neq)
        case nmatch: NotMatchPredicate => visitor.visitNotMatchPredicate(nmatch)
    case QueryNode.PredicateArgNode(arg) =>
      arg match
        case c: CaptureRef => visitor.visitCaptureRef(c)
        case s: StringArg  => visitor.visitStringArg(s)

  def children(node: QueryNode): List[QueryNode] = node match
    case QueryNode.Root(query) =>
      QueryNode.PatternNode(query.root) :: query.predicates.map(QueryNode.PredicateNode(_))
    case QueryNode.PatternNode(pattern) =>
      pattern match
        case NodePattern(_, fields, childPatterns, _, _, _, _) =>
          fields.map(QueryNode.FieldPatternNode(_)) ::: childPatterns.map(QueryNode.PatternNode(_))
        case MultiPattern(patterns) =>
          patterns.map(QueryNode.PatternNode(_))
        case AlternationPattern(patterns, _) =>
          patterns.map(QueryNode.PatternNode(_))
        case WildcardPattern(_) =>
          Nil
    case QueryNode.FieldPatternNode(fieldPattern) =>
      List(QueryNode.PatternNode(fieldPattern.pattern))
    case QueryNode.PredicateNode(predicate) =>
      predicate match
        case EqPredicate(left, right) =>
          List(QueryNode.PredicateArgNode(left), QueryNode.PredicateArgNode(right))
        case MatchPredicate(arg, _) =>
          List(QueryNode.PredicateArgNode(arg))
        case NotEqPredicate(left, right) =>
          List(QueryNode.PredicateArgNode(left), QueryNode.PredicateArgNode(right))
        case NotMatchPredicate(arg, _) =>
          List(QueryNode.PredicateArgNode(arg))
    case QueryNode.PredicateArgNode(_) =>
      Nil

  def count(query: Query): Int =
    foldLeft(query, 0)((acc, _) => acc + 1)

  def foldLeft[A](query: Query, z: A)(f: (A, QueryNode) => A): A =
    def go(node: QueryNode, acc: A): TailRec[A] =
      val updated = f(acc, node)
      val kids    = children(node)
      if kids.isEmpty then done(updated)
      else
        kids.foldLeft(done(updated)) { (tailAcc, child) =>
          for
            a <- tailAcc
            r <- tailcall(go(child, a))
          yield r
        }
    go(QueryNode.Root(query), z).result

  def collect[B](query: Query)(pf: PartialFunction[QueryNode, B]): List[B] =
    val buffer = ArrayBuffer.empty[B]
    foldLeft(query, ()) { (_, node) =>
      if pf.isDefinedAt(node) then buffer += pf(node)
    }
    buffer.toList

  def collectPostOrder[B](query: Query)(pf: PartialFunction[QueryNode, B]): List[B] =
    val buffer                             = ArrayBuffer.empty[B]
    def go(node: QueryNode): TailRec[Unit] =
      val kids     = children(node)
      val kidsWork =
        if kids.isEmpty then done(())
        else
          kids.foldLeft(done(())) { (acc, child) =>
            for
              _ <- acc
              _ <- tailcall(go(child))
            yield ()
          }
      for _ <- kidsWork
      yield if pf.isDefinedAt(node) then buffer += pf(node)
    go(QueryNode.Root(query)).result
    buffer.toList

  def traverse[Ctx, Log, Err](query: Query)(
      f: QueryNode => QueryLogic.QueryEffect[Ctx, Log, Err, Unit]
  ): QueryLogic.QueryEffect[Ctx, Log, Err, Unit] =
    traverseNode(QueryNode.Root(query))(f)

  private def traverseNode[Ctx, Log, Err](node: QueryNode)(
      f: QueryNode => QueryLogic.QueryEffect[Ctx, Log, Err, Unit]
  ): QueryLogic.QueryEffect[Ctx, Log, Err, Unit] =
    for
      _ <- f(node)
      _ <- traverseChildren(QueryVisitor.children(node))(f)
    yield ()

  private def traverseChildren[Ctx, Log, Err](children: List[QueryNode])(
      f: QueryNode => QueryLogic.QueryEffect[Ctx, Log, Err, Unit]
  ): QueryLogic.QueryEffect[Ctx, Log, Err, Unit] =
    children match
      case Nil       => ()
      case h :: rest =>
        for
          _ <- traverseNode(h)(f)
          _ <- traverseChildren(rest)(f)
        yield ()

  extension (query: Query)

    @targetName("extQueryVisit")
    def visit[A](visitor: QueryVisitor[A]): A =
      QueryVisitor.visit(QueryNode.Root(query), visitor)

    @targetName("extQueryFold")
    def fold[A](z: A)(f: (A, QueryNode) => A): A =
      QueryVisitor.foldLeft(query, z)(f)

    @targetName("extQueryCollect")
    def collect[B](pf: PartialFunction[QueryNode, B]): List[B] =
      QueryVisitor.collect(query)(pf)
