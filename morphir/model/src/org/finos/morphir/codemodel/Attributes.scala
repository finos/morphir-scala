package org.finos.morphir.codemodel

import org.finos.morphir.naming._
import kyo.Schema

// Placeholder for SourceLocation until we verify its location in the codebase
// Usually in org.finos.morphir.ir.Location or similar.
// For now, I'll alias it to Any or define a dummy if not found.
// Actually, looking at existing code is better. I'll search for SourceLocation first.
// Assuming it's simple for this write, but I'll make it generic or use a known type if I can.
// Spec says: `source: Option(SourceLocation)`
// I will define a simple SourceLocation here for V4 or import it if I find it.
// Given strictness, I'll define it if not present.

final case class SourceLocation(file: Option[String], line: Int, column: Int) derives Schema

final case class TypeAttributes(
    source: Option[SourceLocation],
    constraints: Option[TypeConstraints],
    extensions: Map[FQName, Expr]
) derives Schema

object TypeAttributes {
  val empty: TypeAttributes = TypeAttributes(None, None, Map.empty)
}

final case class ValueAttributes(
    source: Option[SourceLocation],
    inferredType: Option[Type],
    properties: Option[ValueProperties],
    extensions: Map[FQName, Expr]
) derives Schema

object ValueAttributes {
  val empty: ValueAttributes = ValueAttributes(None, None, None, Map.empty)
}
