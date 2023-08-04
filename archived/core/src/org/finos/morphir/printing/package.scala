package org.finos.morphir

import fansi.{Attrs, Str}
import pprint.{PPrinter, Tree}
import sourcecode.{FileName, Line, Text}

package object printing {

  // TODO: Use Castor to manager customizing this by having a Map from a izumi type tag to a function that works on that tag
  // type Key[A:Tag] => PPrintConfig[A]
  private[printing] val pprintCustom = pprint.copy()
  def log[T](
      x: Text[T],
      tag: String,
      width: Int,
      height: Int,
      indent: Int,
      escapeUnicode: Boolean,
      showFieldNames: Boolean
  )(implicit line: Line, fileName: FileName): T =
    pprintCustom.log(x, tag, width, height, indent, escapeUnicode, showFieldNames)(line, fileName)

  def apply(
      x: Any,
      width: Int,
      height: Int,
      indent: Int,
      initialOffset: Int,
      escapeUnicode: Boolean,
      showFieldNames: Boolean
  ): Str = pprintCustom.apply(x, width, height, indent, initialOffset, escapeUnicode, showFieldNames)

  def pprintln[T](
      x: T,
      width: Int,
      height: Int,
      indent: Int,
      initialOffset: Int,
      escapeUnicode: Boolean,
      showFieldNames: Boolean
  ): Unit = pprintCustom.pprintln(x, width, height, indent, initialOffset, escapeUnicode, showFieldNames)

  def tokenize(
      x: Any,
      width: Int,
      height: Int,
      indent: Int,
      initialOffset: Int,
      escapeUnicode: Boolean,
      showFieldNames: Boolean
  ): Iterator[Str] = pprintCustom.tokenize(x, width, height, indent, initialOffset, escapeUnicode, showFieldNames)

  def copy(
      defaultWidth: Int,
      defaultHeight: Int,
      defaultIndent: Int,
      defaultEscapeUnicode: Boolean,
      defaultShowFieldNames: Boolean,
      colorLiteral: Attrs,
      colorApplyPrefix: Attrs,
      additionalHandlers: PartialFunction[Any, Tree]
  ): PPrinter = pprintCustom.copy(
    defaultWidth,
    defaultHeight,
    defaultIndent,
    defaultEscapeUnicode,
    defaultShowFieldNames,
    colorLiteral,
    colorApplyPrefix,
    additionalHandlers
  )

  def stringify[A: Stringify](value: A): String = Stringify[A].apply(value)

  def stringifyLine[A: Stringify](value: A): String = Stringify[A].apply(value) + System.lineSeparator()
}
