package org.finos
package morphir
package ir

import prelude.*
import upickle.core.{Annotator, Visitor}
import Distribution.Distribution
import Documented.Documented
import Name.Name
import FQName.FQName
import Literal.Literal
import Module.ModuleName
import QName.QName
import Path.Path
import Package.PackageName
import Type.Type
import Value.Value
import ir.Module as M
import ir.Package as P
import ir.Type as T
import ir.Value as V
import ir.AccessControlled

trait IRWriters extends IRValueWriters { self: Annotator =>

  implicit def DocumentedWriter[A: Writer]: Writer[Documented[A]] = macroW

  implicit def ModuleSpecificationWriter[TA: Writer]: Writer[M.Specification[TA]] = new Writer[M.Specification[TA]] {
    override def write0[R](out: Visitor[_, R], v: M.Specification[TA]): R = {
      val ctx = out.visitObject(2, true, -1).narrow
      ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "types"), -1)
      ctx.visitValue(
        implicitly[Writer[List[(Name, Documented[T.Specification[TA]])]]].write(ctx.subVisitor, v.types.toList),
        -1
      )
      ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "values"), -1)
      ctx.visitValue(
        implicitly[Writer[List[(Name, Documented[V.Specification[TA]])]]].write(ctx.subVisitor, v.values.toList),
        -1
      )
      ctx.visitEnd(-1)
    }
  }

  implicit def ModuleDefinitionWriter[TA: Writer, VA: Writer]: Writer[M.Definition[TA, VA]] =
    new Writer[M.Definition[TA, VA]] {
      override def write0[R](out: Visitor[_, R], v: M.Definition[TA, VA]): R = {
        val ctx = out.visitObject(2, true, -1).narrow
        ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "types"), -1)
        ctx.visitValue(
          implicitly[Writer[List[(Name, AccessControlled.AccessControlled[Documented[T.Definition[TA]]])]]]
            .write(ctx.subVisitor, v.types.toList),
          -1
        )
        ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "values"), -1)
        ctx.visitValue(
          implicitly[Writer[List[(Name, AccessControlled.AccessControlled[Documented[V.Definition[TA, VA]]])]]]
            .write(ctx.subVisitor, v.values.toList),
          -1
        )
        ctx.visitEnd(-1)
      }
    }

  implicit def PackageSpecificationWriter[TA: Writer]: Writer[P.Specification[TA]] = new Writer[P.Specification[TA]] {
    override def write0[R](out: Visitor[_, R], v: P.Specification[TA]): R = {
      val ctx = out.visitObject(2, true, -1).narrow
      ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "modules"), -1)
      ctx.visitValue(
        implicitly[Writer[List[(Module.ModuleName, M.Specification[TA])]]].write(ctx.subVisitor, v.modules.toList),
        -1
      )
      ctx.visitEnd(-1)
    }
  }

  implicit def PackageDefinitionWriter[TA: Writer, VA: Writer]: Writer[P.Definition[TA, VA]] =
    new Writer[P.Definition[TA, VA]] {
      override def write0[R](out: Visitor[_, R], v: P.Definition[TA, VA]): R = {
        val ctx = out.visitObject(2, true, -1).narrow
        ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "modules"), -1)
        ctx.visitValue(
          implicitly[Writer[List[(Module.ModuleName, AccessControlled.AccessControlled[M.Definition[TA, VA]])]]]
            .write(ctx.subVisitor, v.modules.toList),
          -1
        )
        ctx.visitEnd(-1)
      }
    }
}

trait IRValueWriters extends IRTypeWriters with LiteralWriters { self: Annotator =>

  implicit def ValueDefinitionParameterWriter[TA: Writer, VA: Writer]: Writer[V.Parameter[TA, VA]] =
    new Writer[V.Parameter[TA, VA]] {
      override def write0[R](out: Visitor[_, R], v: V.Parameter[TA, VA]): R = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v._1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v._2), -1)
        ctx.visitValue(implicitly[Writer[Type[TA]]].write(ctx.subVisitor, v._3), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def ValueSpecParameterWriter[A: Writer]: Writer[V.SpecParameter[A]] = new Writer[V.SpecParameter[A]] {
    override def write0[R](out: Visitor[_, R], v: V.SpecParameter[A]): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v._1), -1)
      ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v._2), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ValueDefinitionWriter[TA: Writer, VA: Writer]: Writer[V.Definition[TA, VA]] = macroW

  implicit def ValueSpecificationWriter[TA: Writer]: Writer[V.Specification[TA]] = macroW

  implicit def AsPatternWriter[A: Writer]: Writer[V.Pattern.AsPattern[A]] = new Writer[V.Pattern.AsPattern[A]] {
    override def write0[V](out: Visitor[_, V], v: V.Pattern.AsPattern[A]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("as_pattern", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[V.Pattern[A]]].write(ctx.subVisitor, v.pattern), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ConstructorPatternWriter[A: Writer]: Writer[V.Pattern.ConstructorPattern[A]] =
    new Writer[V.Pattern.ConstructorPattern[A]] {
      override def write0[V](out: Visitor[_, V], v: V.Pattern.ConstructorPattern[A]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("constructor_pattern", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[FQName]].write(ctx.subVisitor, v.constructorName), -1)
        ctx.visitValue(implicitly[Writer[Chunk[V.Pattern[A]]]].write(ctx.subVisitor, v.argumentPatterns), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def EmptyListPatternWriter[A: Writer]: Writer[V.Pattern.EmptyListPattern[A]] =
    new Writer[V.Pattern.EmptyListPattern[A]] {
      override def write0[V](out: Visitor[_, V], v: V.Pattern.EmptyListPattern[A]): V = {
        val ctx = out.visitArray(2, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("empty_list_pattern", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def HeadTailPatternWriter[A: Writer]: Writer[V.Pattern.HeadTailPattern[A]] =
    new Writer[V.Pattern.HeadTailPattern[A]] {
      override def write0[V](out: Visitor[_, V], v: V.Pattern.HeadTailPattern[A]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("head_tail_pattern", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[V.Pattern[A]]].write(ctx.subVisitor, v.headPattern), -1)
        ctx.visitValue(implicitly[Writer[V.Pattern[A]]].write(ctx.subVisitor, v.tailPattern), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def LiteralPatternWriter[A: Writer]: Writer[V.Pattern.LiteralPattern[A]] =
    new Writer[V.Pattern.LiteralPattern[A]] {
      override def write0[V](out: Visitor[_, V], v: V.Pattern.LiteralPattern[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("literal_pattern", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Literal]].write(ctx.subVisitor, v.literal), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def TuplePatternWriter[A: Writer]: Writer[V.Pattern.TuplePattern[A]] =
    new Writer[V.Pattern.TuplePattern[A]] {
      override def write0[V](out: Visitor[_, V], v: V.Pattern.TuplePattern[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("tuple_pattern", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Chunk[V.Pattern[A]]]].write(ctx.subVisitor, v.elementPatterns), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def UnitPatternWriter[A: Writer]: Writer[V.Pattern.UnitPattern[A]] = new Writer[V.Pattern.UnitPattern[A]] {
    override def write0[V](out: Visitor[_, V], v: V.Pattern.UnitPattern[A]): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("unit_pattern", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def WildcardPatternWriter[A: Writer]: Writer[V.Pattern.WildcardPattern[A]] =
    new Writer[V.Pattern.WildcardPattern[A]] {
      override def write0[V](out: Visitor[_, V], v: V.Pattern.WildcardPattern[A]): V = {
        val ctx = out.visitArray(2, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("wildcard_pattern", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def PatternWriter[A: Writer]: Writer[V.Pattern[A]] = new Writer[V.Pattern[A]] {
    override def write0[R](out: Visitor[_, R], v: V.Pattern[A]): R = v match {
      case r: V.Pattern.AsPattern[A]          => implicitly[Writer[V.Pattern.AsPattern[A]]].write0(out, r)
      case r: V.Pattern.ConstructorPattern[A] => implicitly[Writer[V.Pattern.ConstructorPattern[A]]].write0(out, r)
      case r: V.Pattern.EmptyListPattern[A]   => implicitly[Writer[V.Pattern.EmptyListPattern[A]]].write0(out, r)
      case r: V.Pattern.HeadTailPattern[A]    => implicitly[Writer[V.Pattern.HeadTailPattern[A]]].write0(out, r)
      case r: V.Pattern.LiteralPattern[A]     => implicitly[Writer[V.Pattern.LiteralPattern[A]]].write0(out, r)
      case r: V.Pattern.TuplePattern[A]       => implicitly[Writer[V.Pattern.TuplePattern[A]]].write0(out, r)
      case r: V.Pattern.UnitPattern[A]        => implicitly[Writer[V.Pattern.UnitPattern[A]]].write0(out, r)
      case r: V.Pattern.WildcardPattern[A]    => implicitly[Writer[V.Pattern.WildcardPattern[A]]].write0(out, r)
    }
  }

  implicit def ApplyValueWriter[TA: Writer, VA: Writer]: Writer[V.Apply[TA, VA]] = new Writer[V.Apply[TA, VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Apply[TA, VA]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("apply", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.function), -1)
      ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.argument), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ConstructorValueWriter[VA: Writer]: Writer[V.Constructor[VA]] = new Writer[V.Constructor[VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Constructor[VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("constructor", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[FQName]].write(ctx.subVisitor, v.fullyQualifiedName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def DestructureValueWriter[TA: Writer, VA: Writer]: Writer[V.Destructure[TA, VA]] =
    new Writer[V.Destructure[TA, VA]] {
      override def write0[V](out: Visitor[_, V], v: V.Destructure[TA, VA]): V = {
        val ctx = out.visitArray(5, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("destructure", -1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[V.Pattern[VA]]].write(ctx.subVisitor, v.pattern), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.valueToDestruct), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.inValue), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def FieldValueWriter[TA: Writer, VA: Writer]: Writer[V.Field[TA, VA]] = new Writer[V.Field[TA, VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Field[TA, VA]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("field", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.subjectValue), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.fieldName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def FieldFunctionValueWriter[VA: Writer]: Writer[V.FieldFunction[VA]] = new Writer[V.FieldFunction[VA]] {
    override def write0[V](out: Visitor[_, V], v: V.FieldFunction[VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("field_function", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def IfThenElseValueWriter[TA: Writer, VA: Writer]: Writer[V.IfThenElse[TA, VA]] =
    new Writer[V.IfThenElse[TA, VA]] {
      override def write0[V](out: Visitor[_, V], v: V.IfThenElse[TA, VA]): V = {
        val ctx = out.visitArray(5, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("if_then_else", -1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.condition), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.thenBranch), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.elseBranch), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def LambdaValueWriter[TA: Writer, VA: Writer]: Writer[V.Lambda[TA, VA]] = new Writer[V.Lambda[TA, VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Lambda[TA, VA]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("lambda", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[V.Pattern[VA]]].write(ctx.subVisitor, v.argumentPattern), -1)
      ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.body), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def LetDefinitionValueWriter[TA: Writer, VA: Writer]: Writer[V.LetDefinition[TA, VA]] =
    new Writer[V.LetDefinition[TA, VA]] {
      override def write0[V](out: Visitor[_, V], v: V.LetDefinition[TA, VA]): V = {
        val ctx = out.visitArray(5, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("let_definition", -1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.valueName), -1)
        ctx.visitValue(implicitly[Writer[V.Definition[TA, VA]]].write(ctx.subVisitor, v.valueDefinition), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.inValue), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def LetRecursionValueWriter[TA: Writer, VA: Writer]: Writer[V.LetRecursion[TA, VA]] =
    new Writer[V.LetRecursion[TA, VA]] {
      override def write0[V](out: Visitor[_, V], v: V.LetRecursion[TA, VA]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("let_recursion", -1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(
          implicitly[Writer[Map[Name, V.Definition[TA, VA]]]].write(ctx.subVisitor, v.valueDefinitions),
          -1
        )
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.inValue), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def ListValueWriter[TA: Writer, VA: Writer]: Writer[V.List[TA, VA]] = new Writer[V.List[TA, VA]] {
    override def write0[V](out: Visitor[_, V], v: V.List[TA, VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("list", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Chunk[Value[TA, VA]]]].write(ctx.subVisitor, v.items), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def LiteralValueWriter[VA: Writer]: Writer[V.Literal[VA]] = new Writer[V.Literal[VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Literal[VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("literal", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Literal]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def PatternMatchValueWriter[TA: Writer, VA: Writer]: Writer[V.PatternMatch[TA, VA]] =
    new Writer[V.PatternMatch[TA, VA]] {
      override def write0[V](out: Visitor[_, V], v: V.PatternMatch[TA, VA]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("pattern_match", -1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.branchOutOn), -1)
        ctx.visitValue(
          implicitly[Writer[Chunk[(V.Pattern[VA], Value[TA, VA])]]].write(ctx.subVisitor, v.cases),
          -1
        )
        ctx.visitEnd(-1)
      }
    }

  implicit def RecordValueWriter[TA: Writer, VA: Writer]: Writer[V.Record[TA, VA]] = new Writer[V.Record[TA, VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Record[TA, VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("record", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Chunk[(Name, Value[TA, VA])]]].write(ctx.subVisitor, v.fields), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ReferenceValueWriter[VA: Writer]: Writer[V.Reference[VA]] = new Writer[V.Reference[VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Reference[VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("reference", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[FQName]].write(ctx.subVisitor, v.fullyQualifiedName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def TupleValueWriter[TA: Writer, VA: Writer]: Writer[V.Tuple[TA, VA]] = new Writer[V.Tuple[TA, VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Tuple[TA, VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("tuple", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Chunk[Value[TA, VA]]]].write(ctx.subVisitor, v.elements), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def UnitValueWriter[VA: Writer]: Writer[V.Unit[VA]] = new Writer[V.Unit[VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Unit[VA]): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("unit", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def UpdateRecordValueWriter[TA: Writer, VA: Writer]: Writer[V.UpdateRecord[TA, VA]] =
    new Writer[V.UpdateRecord[TA, VA]] {
      override def write0[V](out: Visitor[_, V], v: V.UpdateRecord[TA, VA]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("update_record", -1), -1)
        ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Value[TA, VA]]].write(ctx.subVisitor, v.valueToUpdate), -1)
        ctx.visitValue(implicitly[Writer[Map[Name, Value[TA, VA]]]].write(ctx.subVisitor, v.fieldsToUpdate), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def VariableValueWriter[VA: Writer]: Writer[V.Variable[VA]] = new Writer[V.Variable[VA]] {
    override def write0[V](out: Visitor[_, V], v: V.Variable[VA]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("variable", -1), -1)
      ctx.visitValue(implicitly[Writer[VA]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ValueWriter[TA: Writer, VA: Writer]: Writer[Value[TA, VA]] = new Writer[Value[TA, VA]] {
    override def write0[R](out: Visitor[_, R], v: Value[TA, VA]): R = v match {
      case r: V.Apply[TA, VA]         => implicitly[Writer[V.Apply[TA, VA]]].write0(out, r)
      case r: V.Constructor[VA]       => implicitly[Writer[V.Constructor[VA]]].write0(out, r)
      case r: V.Destructure[TA, VA]   => implicitly[Writer[V.Destructure[TA, VA]]].write0(out, r)
      case r: V.Field[TA, VA]         => implicitly[Writer[V.Field[TA, VA]]].write0(out, r)
      case r: V.FieldFunction[VA]     => implicitly[Writer[V.FieldFunction[VA]]].write0(out, r)
      case r: V.IfThenElse[TA, VA]    => implicitly[Writer[V.IfThenElse[TA, VA]]].write0(out, r)
      case r: V.Lambda[TA, VA]        => implicitly[Writer[V.Lambda[TA, VA]]].write0(out, r)
      case r: V.LetDefinition[TA, VA] => implicitly[Writer[V.LetDefinition[TA, VA]]].write0(out, r)
      case r: V.LetRecursion[TA, VA]  => implicitly[Writer[V.LetRecursion[TA, VA]]].write0(out, r)
      case r: V.List[TA, VA]          => implicitly[Writer[V.List[TA, VA]]].write0(out, r)
      case r: V.Literal[VA]           => implicitly[Writer[V.Literal[VA]]].write0(out, r)
      case r: V.PatternMatch[TA, VA]  => implicitly[Writer[V.PatternMatch[TA, VA]]].write0(out, r)
      case r: V.Record[TA, VA]        => implicitly[Writer[V.Record[TA, VA]]].write0(out, r)
      case r: V.Reference[VA]         => implicitly[Writer[V.Reference[VA]]].write0(out, r)
      case r: V.Tuple[TA, VA]         => implicitly[Writer[V.Tuple[TA, VA]]].write0(out, r)
      case r: V.Unit[VA]              => implicitly[Writer[V.Unit[VA]]].write0(out, r)
      case r: V.UpdateRecord[TA, VA]  => implicitly[Writer[V.UpdateRecord[TA, VA]]].write0(out, r)
      case r: V.Variable[VA]          => implicitly[Writer[V.Variable[VA]]].write0(out, r)
    }
  }
}

trait IRTypeWriters extends NamingWriters { self: Annotator =>

  implicit def ExtensibleRecordTypeWriter[A: Writer]: Writer[T.Type.ExtensibleRecord[A]] =
    new Writer[T.Type.ExtensibleRecord[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Type.ExtensibleRecord[A]): V = {
        val ctx = out.visitArray(4, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("ExtensibleRecord", -1), -1)
        ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
        ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.name), -1)
        ctx.visitValue(implicitly[Writer[List[T.Field[A]]]].write(ctx.subVisitor, v.fields), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def FunctionTypeWriter[A: Writer]: Writer[T.Type.Function[A]] = new Writer[T.Type.Function[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Function[A]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Function", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.argumentType), -1)
      ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.returnType), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def RecordTypeWriter[A: Writer]: Writer[T.Type.Record[A]] = new Writer[T.Type.Record[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Record[A]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Record", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[List[T.Field[A]]]].write(ctx.subVisitor, v.fields), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def ReferenceTypeWriter[A: Writer]: Writer[T.Type.Reference[A]] = new Writer[T.Type.Reference[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Reference[A]): V = {
      val ctx = out.visitArray(4, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Reference", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[FQName]].write(ctx.subVisitor, v.typeName), -1)
      ctx.visitValue(implicitly[Writer[List[Type[A]]]].write(ctx.subVisitor, v.typeParams), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def TupleTypeWriter[A: Writer]: Writer[T.Type.Tuple[A]] = new Writer[T.Type.Tuple[A]] {
    override def write0[V](out: Visitor[_, V], v: T.Type.Tuple[A]): V = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Tuple", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[List[Type[A]]]].write(ctx.subVisitor, v.elements), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def UnitTypeWriter[A: Writer]: Writer[T.Type.Unit[A]] = new Writer[T.Type.Unit[A]] {
    def write0[R](out: Visitor[_, R], v: T.Type.Unit[A]): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Unit", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def VariableTypeWriter[A: Writer]: Writer[T.Type.Variable[A]] = new Writer[T.Type.Variable[A]] {
    override def write0[R](out: Visitor[_, R], v: Type.Variable[A]): R = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("Variable", -1), -1)
      ctx.visitValue(implicitly[Writer[A]].write(ctx.subVisitor, v.attributes), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def TypeWriter[A: Writer]: Writer[Type[A]] = new Writer[Type[A]] {
    override def write0[R](out: Visitor[_, R], v: Type[A]): R = v match {
      case r: T.Type.ExtensibleRecord[A] => implicitly[Writer[T.Type.ExtensibleRecord[A]]].write0(out, r)
      case r: T.Type.Function[A]         => implicitly[Writer[T.Type.Function[A]]].write0(out, r)
      case r: T.Type.Record[A]           => implicitly[Writer[T.Type.Record[A]]].write0(out, r)
      case r: T.Type.Reference[A]        => implicitly[Writer[T.Type.Reference[A]]].write0(out, r)
      case r: T.Type.Tuple[A]            => implicitly[Writer[T.Type.Tuple[A]]].write0(out, r)
      case r: T.Type.Unit[A]             => implicitly[Writer[T.Type.Unit[A]]].write0(out, r)
      case r: T.Type.Variable[A]         => implicitly[Writer[T.Type.Variable[A]]].write0(out, r)
    }
  }

  implicit def FieldTypeWriter[A: Writer]: Writer[T.Field[A]] = macroW

  implicit def ConstructorsWriter[A: Writer]: Writer[T.Constructors[A]] = new Writer[T.Constructors[A]] {
    override def write0[R](out: Visitor[_, R], v: T.Constructors[A]): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow

      val cnstrs: List[(Name, List[(Name, Type[A])])] =
        v.byName.values.toList.map(constr => (constr.name, constr.args.args.map(ca => (ca.name, ca.tpe))))

      cnstrs.toList.foreach { case (name, constructor) =>
        val ctx2 = ctx.subVisitor.visitArray(2, -1).narrow
        ctx2.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, name), -1)
        ctx2.visitValue(implicitly[Writer[List[(Name, Type[A])]]].write(ctx.subVisitor, constructor), -1)
        ctx2.visitEnd(-1)
      }
      ctx.visitEnd(-1)
    }
  }

  implicit val AccessWriter: Writer[AccessControlled.Access] = new Writer[AccessControlled.Access] {
    def write0[R](out: Visitor[_, R], v: AccessControlled.Access): R = {
      val access = v match {
        case AccessControlled.Access.Public  => "Public"
        case AccessControlled.Access.Private => "Private"
      }
      out.visitString(access, -1)
    }
  }

  implicit def AccessControlledWriter[A: Writer]: Writer[AccessControlled.AccessControlled[A]] = macroW

  implicit def TypeAliasDefinitionWriter[A: Writer]: Writer[T.Definition.TypeAliasDefinition[A]] =
    new Writer[T.Definition.TypeAliasDefinition[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Definition.TypeAliasDefinition[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("TypeAliasDefinition", -1), -1)
        ctx.visitValue(implicitly[Writer[Chunk[Name]]].write(ctx.subVisitor, v.typeParams), -1)
        ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.typeExpr), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def CustomTypeDefinitionWriter[A: Writer]: Writer[T.Definition.CustomTypeDefinition[A]] =
    new Writer[T.Definition.CustomTypeDefinition[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Definition.CustomTypeDefinition[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("CustomTypeDefinition", -1), -1)
        ctx.visitValue(implicitly[Writer[Chunk[Name]]].write(ctx.subVisitor, v.typeParams), -1)
        ctx.visitValue(
          implicitly[Writer[AccessControlled.AccessControlled[T.Constructors[A]]]].write(ctx.subVisitor, v.ctors),
          -1
        )
        ctx.visitEnd(-1)
      }
    }

  implicit def TypeDefinitionWriter[A: Writer]: Writer[T.Definition[A]] = new Writer[T.Definition[A]] {
    override def write0[R](out: Visitor[_, R], v: T.Definition[A]): R = v match {
      case r: T.Definition.TypeAliasDefinition[A] =>
        implicitly[Writer[T.Definition.TypeAliasDefinition[A]]].write0(out, r)
      case r: T.Definition.CustomTypeDefinition[A] =>
        implicitly[Writer[T.Definition.CustomTypeDefinition[A]]].write0(out, r)
    }
  }

  implicit def TypeAliasSpecificationWriter[A: Writer]: Writer[T.Specification.TypeAliasSpecification[A]] =
    new Writer[T.Specification.TypeAliasSpecification[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Specification.TypeAliasSpecification[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("TypeAliasSpecification", -1), -1)
        ctx.visitValue(implicitly[Writer[List[Name]]].write(ctx.subVisitor, v.typeParams), -1)
        ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.expr), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def CustomTypeSpecificationWriter[A: Writer]: Writer[T.Specification.CustomTypeSpecification[A]] =
    new Writer[T.Specification.CustomTypeSpecification[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Specification.CustomTypeSpecification[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("CustomTypeSpecification", -1), -1)
        ctx.visitValue(implicitly[Writer[List[Name]]].write(ctx.subVisitor, v.typeParams), -1)
        ctx.visitValue(
          implicitly[Writer[T.Constructors[A]]].write(ctx.subVisitor, v.ctors),
          -1
        )
        ctx.visitEnd(-1)
      }
    }

  implicit val OpaqueTypeSpecificationWriter: Writer[T.Specification.OpaqueTypeSpecification] =
    new Writer[T.Specification.OpaqueTypeSpecification] {
      override def write0[V](out: Visitor[_, V], v: T.Specification.OpaqueTypeSpecification): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("OpaqueTypeSpecification", -1), -1)
        ctx.visitValue(implicitly[Writer[List[Name]]].write(ctx.subVisitor, v.typeParams), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def DerivedTypePropertiesWriter[A: Writer]: Writer[T.Specification.Properties.DerivedType[A]] =
    new Writer[T.Specification.Properties.DerivedType[A]] {
      override def write0[R](out: Visitor[_, R], v: T.Specification.Properties.DerivedType[A]): R = {
        val ctx = out.visitObject(3, true, -1).narrow
        ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "baseType"), -1)
        ctx.visitValue(implicitly[Writer[Type[A]]].write(ctx.subVisitor, v.baseType), -1)
        ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "fromBaseType"), -1)
        ctx.visitValue(implicitly[Writer[FQName]].write(ctx.subVisitor, v.fromBaseType), -1)
        ctx.visitKeyValue(implicitly[Writer[String]].write(ctx.visitKey(-1), "toBaseType"), -1)
        ctx.visitValue(implicitly[Writer[FQName]].write(ctx.subVisitor, v.toBaseType), -1)
        ctx.visitEnd(-1)
      }
    }

  implicit def DerivedTypeSpecificationWriter[A: Writer]: Writer[T.Specification.DerivedTypeSpecification[A]] =
    new Writer[T.Specification.DerivedTypeSpecification[A]] {
      override def write0[V](out: Visitor[_, V], v: T.Specification.DerivedTypeSpecification[A]): V = {
        val ctx = out.visitArray(3, -1).narrow
        ctx.visitValue(ctx.subVisitor.visitString("DerivedTypeSpecification", -1), -1)
        ctx.visitValue(implicitly[Writer[List[Name]]].write(ctx.subVisitor, v.typeParams), -1)
        ctx.visitValue(
          implicitly[Writer[T.Specification.Properties.DerivedType[A]]].write(ctx.subVisitor, v.derivationProps),
          -1
        )
        ctx.visitEnd(-1)
      }
    }

  implicit def TypeSpecificationWriter[A: Writer]: Writer[T.Specification[A]] = new Writer[T.Specification[A]] {
    override def write0[R](out: Visitor[_, R], v: T.Specification[A]): R = v match {
      case r: T.Specification.TypeAliasSpecification[A] =>
        implicitly[Writer[T.Specification.TypeAliasSpecification[A]]].write0(out, r)
      case r: T.Specification.CustomTypeSpecification[A] =>
        implicitly[Writer[T.Specification.CustomTypeSpecification[A]]].write0(out, r)
      case r: T.Specification.OpaqueTypeSpecification =>
        implicitly[Writer[T.Specification.OpaqueTypeSpecification]].write0(out, r)
      case r: T.Specification.DerivedTypeSpecification[A] =>
        implicitly[Writer[T.Specification.DerivedTypeSpecification[A]]].write0(out, r)
    }
  }
}

trait NamingWriters extends upickle.implicits.Writers { self: Annotator =>

  implicit val NameWriter: Writer[Name] = new Writer[Name] {
    def write0[R](out: Visitor[_, R], v: Name): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow
      v.toList.foreach { str =>
        ctx.visitValue(ctx.subVisitor.visitString(str, -1), -1)
      }
      ctx.visitEnd(-1)
    }
  }

  implicit val PathWriter: Writer[Path] = new Writer[Path] {
    def write0[R](out: Visitor[_, R], v: Path): R = {
      val ctx = out.visitArray(v.toList.length, -1).narrow
      v.toList.foreach { name =>
        ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, name), -1)
      }
      ctx.visitEnd(-1)
    }
  }

  implicit val PackageNameWriter: Writer[PackageName] =
    implicitly[Writer[Path]].comap[PackageName](_.toPath)

  implicit val QNameWriter: Writer[QName] = new Writer[QName] {
    def write0[R](out: Visitor[_, R], v: QName): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(implicitly[Writer[Path]].write(ctx.subVisitor, v.modulePath), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.localName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit val FQNameWriter: Writer[FQName] = new Writer[FQName] {
    def write0[R](out: Visitor[_, R], v: FQName): R = {
      val ctx = out.visitArray(3, -1).narrow
      ctx.visitValue(implicitly[Writer[PackageName]].write(ctx.subVisitor, v.packagePath), -1)
      ctx.visitValue(implicitly[Writer[Path]].write(ctx.subVisitor, v.modulePath), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.localName), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit val ModuleNameWriter: Writer[ModuleName] = new Writer[ModuleName] {
    def write0[R](out: Visitor[_, R], v: ModuleName): R = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(implicitly[Writer[Path]].write(ctx.subVisitor, v.namespace.toPath), -1)
      ctx.visitValue(implicitly[Writer[Name]].write(ctx.subVisitor, v.name), -1)
      ctx.visitEnd(-1)
    }
  }
}

trait LiteralWriters extends upickle.implicits.Writers { self: Annotator =>

  implicit def ChunkWriter[A: Writer]: Writer[Chunk[A]] = implicitly[Writer[List[A]]].comap[Chunk[A]](_.toList)

  implicit def BoolLiteralWriter: Writer[Literal.BoolLiteral] = new Writer[Literal.BoolLiteral] {
    override def write0[V](out: Visitor[_, V], v: Literal.BoolLiteral): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("BoolLiteral", -1), -1)
      ctx.visitValue(implicitly[Writer[Boolean]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def CharLiteralWriter: Writer[Literal.CharLiteral] = new Writer[Literal.CharLiteral] {
    override def write0[V](out: Visitor[_, V], v: Literal.CharLiteral): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("CharLiteral", -1), -1)
      ctx.visitValue(implicitly[Writer[Char]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def DecimalLiteralWriter: Writer[Literal.DecimalLiteral] = new Writer[Literal.DecimalLiteral] {
    override def write0[V](out: Visitor[_, V], v: Literal.DecimalLiteral): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("DecimalLiteral", -1), -1)
      ctx.visitValue(implicitly[Writer[BigDecimal]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def FloatLiteralWriter: Writer[Literal.FloatLiteral] = new Writer[Literal.FloatLiteral] {
    override def write0[V](out: Visitor[_, V], v: Literal.FloatLiteral): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("FloatLiteral", -1), -1)
      ctx.visitValue(implicitly[Writer[Double]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def StringLiteralWriter: Writer[Literal.StringLiteral] = new Writer[Literal.StringLiteral] {
    override def write0[V](out: Visitor[_, V], v: Literal.StringLiteral): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("StringLiteral", -1), -1)
      ctx.visitValue(implicitly[Writer[String]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def WholeNumberLiteralWriter: Writer[Literal.WholeNumberLiteral] = new Writer[Literal.WholeNumberLiteral] {
    override def write0[V](out: Visitor[_, V], v: Literal.WholeNumberLiteral): V = {
      val ctx = out.visitArray(2, -1).narrow
      ctx.visitValue(ctx.subVisitor.visitString("WholeNumberLiteral", -1), -1)
      ctx.visitValue(implicitly[Writer[Long]].write(ctx.subVisitor, v.value), -1)
      ctx.visitEnd(-1)
    }
  }

  implicit def LiteralWriter: Writer[Literal] = new Writer[Literal] {
    override def write0[R](out: Visitor[_, R], v: Literal): R = v match {
      case r: Literal.BoolLiteral        => implicitly[Writer[Literal.BoolLiteral]].write0(out, r)
      case r: Literal.CharLiteral        => implicitly[Writer[Literal.CharLiteral]].write0(out, r)
      case r: Literal.DecimalLiteral     => implicitly[Writer[Literal.DecimalLiteral]].write0(out, r)
      case r: Literal.FloatLiteral       => implicitly[Writer[Literal.FloatLiteral]].write0(out, r)
      case r: Literal.StringLiteral      => implicitly[Writer[Literal.StringLiteral]].write0(out, r)
      case r: Literal.WholeNumberLiteral => implicitly[Writer[Literal.WholeNumberLiteral]].write0(out, r)
    }
  }
}
