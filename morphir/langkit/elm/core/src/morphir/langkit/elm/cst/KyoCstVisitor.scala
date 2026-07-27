package morphir.langkit.elm.cst

import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.trees.KyoQueryableTree
import kyo.*

object KyoCstVisitor:

  def visit[S](root: CstNode)(f: CstNode => Unit < S)(using frame: Frame): Unit < S =
    KyoQueryableTree.traverseKyo(root)(f)

  def fold[A, S](root: CstNode, zero: A)(f: (A, CstNode) => A < S)(using frame: Frame): A < S =
    KyoQueryableTree.foldKyo(root, zero)(f)
