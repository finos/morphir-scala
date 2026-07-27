package morphir.langkit.elm.cst

import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.trees.unist.UnistProjection
import morphir.langkit.trees.unist.UnistSpan

object CstUnistProjection:

  given projection: UnistProjection[CstNode] with

    def span(t: CstNode): Option[UnistSpan] =
      Some(UnistSpan.fromOffsetLength(t.span.offset, t.span.length))

  export CstQueryableTree.given
