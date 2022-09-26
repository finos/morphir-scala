package org.finos
package morphir
package tools.backend.mir

import dotty.tools.dotc.core._
import Contexts._
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span
import morphir.mir

class MirPositions()(using Context):
  given fromSourcePosition: Conversion[SourcePosition, mir.Position] =
    sourcePos => sourceAndSpanToMirPos(sourcePos.source, sourcePos.span)

  given fromSpan: Conversion[Span, mir.Position] =
    sourceAndSpanToMirPos(ctx.compilationUnit.source, _)

  private def sourceAndSpanToMirPos(
      source: SourceFile,
      span: Span
  ): mir.Position =
    def mirSource = conversionCache.toMIRSource(source)
    if (span.exists && source.exists)
      val point  = span.point
      val line   = source.offsetToLine(point)
      val column = source.column(point)
      mir.Position(mirSource, line, column)
    else if (source.exists) mir.Position(mirSource, 0, 0)
    else mir.Position.NoPosition

  private object conversionCache:
    import dotty.tools.dotc.util._
    private var lastDotcSource: SourceFile             = _
    private var lastMIRSource: mir.Position.SourceFile = _

    def toMIRSource(dotcSource: SourceFile): mir.Position.SourceFile =
      if (dotcSource != lastDotcSource)
        lastMIRSource = convert(dotcSource)
        lastDotcSource = dotcSource
      end if
      lastMIRSource

    private def convert(dotcSource: SourceFile): mir.Position.SourceFile =
      dotcSource.file.file match
        case null =>
          new java.net.URI(
            "virtualfile",        // Pseudo-Scheme
            dotcSource.file.path, // Scheme specific part
            null                  // Fragment
          )
        case file => file.toURI
