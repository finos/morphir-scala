package org.finos
package morphir
package ir

trait MorphirVisitor[-TA, -VA, -In, +Out] extends upickle.core.Visitor[In, Out] {
  def visitName(parts: List[CharSequence], index: Int): Out
}

trait NameVisitor[-In, +Out] {
  def subVisitor: MorphirVisitor[Any, Any, In, Out]
  def visitSegment(value: String, index: Int): Unit
  def done: Out
}

trait ValueNodeVisitor[-TA, -VA, -In, +Out] {
  import extras.ValueTag
  def visitTag(tag: ValueTag, index: Int): Unit
  def visitAttributes(index: Int): MorphirVisitor[TA, VA, In, Out]
  def visitValue(value: Any, index: Int): Unit
  def done: Out
}
