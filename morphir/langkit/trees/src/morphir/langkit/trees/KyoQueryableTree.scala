package morphir.langkit.trees

import kyo.*

object KyoQueryableTree:

  def traverseKyo[T, S](tree: T)(f: T => Unit < S)(using q: QueryableTree[T], frame: Frame): Unit < S =
    f(tree).flatMap(_ => visitChildren(q.children(tree).toList)(child => traverseKyo(child)(f)))

  def foldKyo[T, A, S](tree: T, zero: A)(f: (A, T) => A < S)(using q: QueryableTree[T], frame: Frame): A < S =
    f(zero, tree).flatMap(acc1 => foldChildren(q.children(tree).toList, acc1)(f))

  private def visitChildren[T, S](children: List[T])(
      f: T => Unit < S
  )(using q: QueryableTree[T], frame: Frame): Unit < S =
    children match
      case Nil       => ()
      case h :: rest => f(h).flatMap(_ => visitChildren(rest)(f))

  private def foldChildren[T, A, S](children: List[T], acc: A)(
      f: (A, T) => A < S
  )(using q: QueryableTree[T], frame: Frame): A < S =
    children match
      case Nil       => acc
      case h :: rest => foldKyo(h, acc)(f).flatMap(next => foldChildren(rest, next)(f))
