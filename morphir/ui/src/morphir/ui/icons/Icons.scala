package morphir.ui.icons

import kyo.*
import kyo.UI.*

/**
 * Stroke-drawn 16px glyphs used by the shell chrome. All draw in `currentColor`, so buttons recolor them through CSS
 * alone.
 */
object Icons:

  /** Left-sidebar toggle: panel outline with a list-marked left column (T3-code style). */
  def sidebar: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedRect,
      strokedLine(9.5, 3, 9.5, 21),
      strokedLine(5.5, 8, 7, 8),
      strokedLine(5.5, 12, 7, 12)
    )

  /** Right-panel toggle: panel outline with the divider on the right. */
  def panelRight: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedRect,
      strokedLine(14.5, 3, 14.5, 21)
    )

  /** Bottom-panel toggle: panel outline with the divider along the bottom. */
  def panelBottom: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedRect,
      strokedLine(3, 14.5, 21, 14.5)
    )

  /** Lucide `settings` glyph. */
  def gear: UI =
    Svg.svg
      .viewBox(Svg.ViewBox(0, 0, 24, 24))
      .width(16)
      .height(16)(
        Svg.path
          .fill(Svg.Paint.None)
          .stroke(Svg.Paint.CurrentColor)
          .strokeWidth(2.0)
          .strokeLinecap(Svg.StrokeLinecap.Round)
          .strokeLinejoin(Svg.StrokeLinejoin.Round)
          .d(
            Svg.PathData.raw(
              "M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"
            )
          ),
        Svg.circle
          .fill(Svg.Paint.None)
          .stroke(Svg.Paint.CurrentColor)
          .strokeWidth(2.0)
          .cx(12)
          .cy(12)
          .r(3)
      )

  /** Lucide `arrow-left` glyph: leave the settings surface. */
  def back: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedLine(19, 12, 5, 12),
      Svg.path
        .fill(Svg.Paint.None)
        .stroke(Svg.Paint.CurrentColor)
        .strokeWidth(1.6)
        .strokeLinecap(Svg.StrokeLinecap.Round)
        .strokeLinejoin(Svg.StrokeLinejoin.Round)
        .d(Svg.PathData.raw("M12 19l-7-7 7-7"))
    )

  /** Lucide `rotate-ccw` glyph: put settings back to their defaults. */
  def restore: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(15).height(15)(
      Svg.path
        .fill(Svg.Paint.None)
        .stroke(Svg.Paint.CurrentColor)
        .strokeWidth(1.7)
        .strokeLinecap(Svg.StrokeLinecap.Round)
        .strokeLinejoin(Svg.StrokeLinejoin.Round)
        .d(Svg.PathData.raw("M3 12a9 9 0 1 0 3-6.7L3 8")),
      Svg.path
        .fill(Svg.Paint.None)
        .stroke(Svg.Paint.CurrentColor)
        .strokeWidth(1.7)
        .strokeLinecap(Svg.StrokeLinecap.Round)
        .strokeLinejoin(Svg.StrokeLinejoin.Round)
        .d(Svg.PathData.raw("M3 3v5h5"))
    )

  private def strokedRect: Svg.Rect =
    Svg.rect
      .fill(Svg.Paint.None)
      .stroke(Svg.Paint.CurrentColor)
      .strokeWidth(1.6)
      .x(3)
      .y(3)
      .width(18)
      .height(18)
      .rx(3)

  private def strokedLine(x1: Double, y1: Double, x2: Double, y2: Double): Svg.Line =
    Svg.line
      .stroke(Svg.Paint.CurrentColor)
      .strokeWidth(1.6)
      .strokeLinecap(Svg.StrokeLinecap.Round)
      .x1(x1)
      .y1(y1)
      .x2(x2)
      .y2(y2)
