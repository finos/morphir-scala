package org.finos.morphir
package ir
package v4

trait Visitor[Context, +Result] {
  def visitType(cursor: TypeCursor, context: Context): Result
  def visitValue(cursor: ValueCursor, context: Context): Result
}

object Visitor {
  // Example simplistic fold that uses the cursor to traverse
  // This is just a sketch; real traversal strategies would be more complex
  def foldType[Context, Result](cursor: TypeCursor, context: Context)(visitor: Visitor[Context, Result]): Result =
    visitor.visitType(cursor, context)

  def foldValue[Context, Result](cursor: ValueCursor, context: Context)(visitor: Visitor[Context, Result]): Result =
    visitor.visitValue(cursor, context)
}
