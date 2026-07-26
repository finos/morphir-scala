package morphir.langkit.elm.ast

import morphir.langkit.elm.ast.AstQueryableTree.given
import morphir.langkit.trees.KyoQueryableTree
import kyo.*

object KyoAstVisitor:

  def visit[S](root: AstNode)(f: AstNode => Unit < S)(using frame: Frame): Unit < S =
    KyoQueryableTree.traverseKyo(root)(f)

  def fold[A, S](root: AstNode, zero: A)(f: (A, AstNode) => A < S)(using frame: Frame): A < S =
    KyoQueryableTree.foldKyo(root, zero)(f)
