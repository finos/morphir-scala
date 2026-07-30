package org.finos.morphir.codemodel.lowering

import kyo.Chunk
import org.finos.morphir.naming.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.ir.Type as v3

/**
 * Lowers the v3 IR (`org.finos.morphir.ir`, `morphir-ir.json`) to the code model (`org.finos.morphir.codemodel`, the v4
 * IR).
 *
 * v3's `Type[+A]` and `Value[+TA, +VA]` are polymorphic over attributes; the code model's `TypeAttributes` and
 * `ValueAttributes` are fixed records. Attributes are therefore erased, not translated: lowering a `TypedValue` (whose
 * value-level attribute is a `UType`) populates `ValueAttributes.inferredType`; lowering anything else (a `RawValue`,
 * or a type-level attribute, which is always `Unit` in this codebase) produces `empty`.
 *
 * Direction is v3 -> code model only. The reverse is lossy by construction: `Hole`, `Incompleteness`, `Native`,
 * `NativeInfo`, `External`, `EntryPoint`, `TypeConstraints` and `ValueProperties` have no v3 counterpart and are never
 * populated here.
 */
object V3Lowering:

  // ---------------------------------------------------------------------------------------------------------------
  // Types
  // ---------------------------------------------------------------------------------------------------------------

  def lowerType(t: v3.Type[Any]): cm.Type =
    val attrs = cm.TypeAttributes.empty
    t match
      case v3.Type.Unit(_)                => cm.Type.Unit(attrs)
      case v3.Type.Variable(_, name)      => cm.Type.Variable(attrs, name)
      case v3.Type.Reference(_, fq, args) => cm.Type.Reference(attrs, fq, Chunk.from(args.map(lowerType)))
      case v3.Type.Tuple(_, elements)     => cm.Type.Tuple(attrs, Chunk.from(elements.map(lowerType)))
      case v3.Type.Function(_, arg, ret)  => cm.Type.Function(attrs, lowerType(arg), lowerType(ret))
      case v3.Type.Record(_, fields)      =>
        cm.Type.Record(attrs, Chunk.from(fields.map(f => cm.Field(f.name, lowerType(f.data)))))
      case v3.Type.ExtensibleRecord(_, variable, fields) =>
        cm.Type.ExtensibleRecord(attrs, variable, Chunk.from(fields.map(f => cm.Field(f.name, lowerType(f.data)))))
