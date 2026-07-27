package morphir.langkit.elm.ast

import morphir.langkit.elm.ast.AstQueryableTree.given
import morphir.langkit.trees.unist.UnistProjection
import morphir.langkit.trees.unist.UnistSpan

object AstUnistProjection:

  given projection: UnistProjection[AstNode] with

    def span(t: AstNode): Option[UnistSpan] =
      Some(UnistSpan.fromOffsetLength(t.span.offset, t.span.length))

  export AstQueryableTree.given
