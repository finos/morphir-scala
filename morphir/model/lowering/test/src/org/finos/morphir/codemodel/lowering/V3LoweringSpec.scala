package org.finos.morphir.codemodel.lowering

import kyo.test.*
import kyo.Chunk
import zio.Chunk as ZChunk
import org.finos.morphir.naming.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.ir.Type as v3
import org.finos.morphir.ir.Value as v3Value
import org.finos.morphir.ir.Literal as v3Literal
import org.finos.morphir.ir.Module as v3Module
import org.finos.morphir.ir.AccessControlled
import org.finos.morphir.ir.packages.Definition as V3PackageDefinition
import org.finos.morphir.ir.distribution.Distribution as V3Distribution

class V3LoweringSpec extends Test[Any]:

  // -------------------------------------------------------------------------------------------------------------
  // Types
  // -------------------------------------------------------------------------------------------------------------

  "lowers a Unit type" in
    assert(V3Lowering.lowerType(v3.Type.Unit(())) == cm.Type.Unit(cm.TypeAttributes.empty))

  "lowers a Variable type, preserving the name" in {
    val name = Name.fromString("a")
    assert(
      V3Lowering.lowerType(v3.Type.Variable((), name)) ==
        cm.Type.Variable(cm.TypeAttributes.empty, name)
    )
  }

  "lowers a Reference with arguments recursively" in {
    val fq  = FQName.fromString("Morphir.SDK:Basics:maybe")
    val in  = v3.Type.Reference((), fq, List(v3.Type.Unit(())))
    val out = cm.Type.Reference(cm.TypeAttributes.empty, fq, Chunk(cm.Type.Unit(cm.TypeAttributes.empty)))
    assert(V3Lowering.lowerType(in) == out)
  }

  "lowers a Record type's fields" in {
    val in = v3.Type.Record((), List(v3.field("value", v3.Type.Unit(()))))
    assert(
      V3Lowering.lowerType(in) ==
        cm.Type.Record(
          cm.TypeAttributes.empty,
          Chunk(cm.Field(Name.fromString("value"), cm.Type.Unit(cm.TypeAttributes.empty)))
        )
    )
  }

  "lowers a Function type" in {
    val in = v3.Type.Function((), v3.Type.Unit(()), v3.Type.Unit(()))
    assert(
      V3Lowering.lowerType(in) ==
        cm.Type.Function(
          cm.TypeAttributes.empty,
          cm.Type.Unit(cm.TypeAttributes.empty),
          cm.Type.Unit(cm.TypeAttributes.empty)
        )
    )
  }

  // -------------------------------------------------------------------------------------------------------------
  // Literals
  // -------------------------------------------------------------------------------------------------------------

  "lowers every literal case" in {
    assert(V3Lowering.lowerLiteral(v3Literal.Literal.boolean(true)) == cm.Literal.BoolLiteral(true))
    assert(V3Lowering.lowerLiteral(v3Literal.Literal.char('x')) == cm.Literal.CharLiteral("x"))
    assert(V3Lowering.lowerLiteral(v3Literal.Literal.string("hi")) == cm.Literal.StringLiteral("hi"))
    assert(V3Lowering.lowerLiteral(v3Literal.Literal.int(1)) == cm.Literal.IntegerLiteral(BigInt(1)))
    assert(V3Lowering.lowerLiteral(v3Literal.Literal.float(1.5)) == cm.Literal.FloatLiteral(1.5))
    assert(V3Lowering.lowerLiteral(v3Literal.Literal.decimal(BigDecimal(1.5))) ==
      cm.Literal.DecimalLiteral(BigDecimal(1.5)))
  }

  // -------------------------------------------------------------------------------------------------------------
  // Expressions
  // -------------------------------------------------------------------------------------------------------------

  "lowers a literal expression" in {
    val in  = v3Value.Value.Literal((), v3Literal.Lit.int(1))
    val out = cm.Expr.Literal(cm.ValueAttributes.empty, cm.Literal.IntegerLiteral(BigInt(1)))
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers an apply expression recursively" in {
    val fq = FQName.fromString("Morphir.SDK:Basics:add")
    val in = v3Value.Value.Apply(
      (),
      v3Value.Value.Reference((), fq),
      v3Value.Value.Unit(())
    )
    val out = cm.Expr.Apply(
      cm.ValueAttributes.empty,
      cm.Expr.Reference(cm.ValueAttributes.empty, fq),
      cm.Expr.Unit(cm.ValueAttributes.empty)
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a Variable expression" in {
    val name = Name.fromString("x")
    assert(V3Lowering.lowerExpr(v3Value.Value.Variable((), name)) == cm.Expr.Variable(cm.ValueAttributes.empty, name))
  }

  "lowers a Constructor expression" in {
    val fq = FQName.fromString("Morphir.SDK:Maybe:just")
    assert(V3Lowering.lowerExpr(v3Value.Value.Constructor((), fq)) == cm.Expr.Constructor(cm.ValueAttributes.empty, fq))
  }

  "lowers a Tuple expression recursively" in {
    val in  = v3Value.Value.Tuple((), ZChunk(v3Value.Value.Unit(()), v3Value.Value.Unit(())))
    val out = cm.Expr.Tuple(
      cm.ValueAttributes.empty,
      Chunk(cm.Expr.Unit(cm.ValueAttributes.empty), cm.Expr.Unit(cm.ValueAttributes.empty))
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a List expression recursively" in {
    val in  = v3Value.Value.List((), ZChunk(v3Value.Value.Unit(())))
    val out = cm.Expr.List(cm.ValueAttributes.empty, Chunk(cm.Expr.Unit(cm.ValueAttributes.empty)))
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a Record expression's fields" in {
    val name = Name.fromString("field1")
    val in   = v3Value.Value.Record((), ZChunk((name, v3Value.Value.Unit(()))))
    val out  =
      cm.Expr.Record(cm.ValueAttributes.empty, Chunk(cm.RecordField(name, cm.Expr.Unit(cm.ValueAttributes.empty))))
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a Field expression" in {
    val name = Name.fromString("field1")
    val in   = v3Value.Value.Field((), v3Value.Value.Unit(()), name)
    val out  = cm.Expr.Field(cm.ValueAttributes.empty, cm.Expr.Unit(cm.ValueAttributes.empty), name)
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a FieldFunction expression" in {
    val name = Name.fromString("field1")
    assert(V3Lowering.lowerExpr(v3Value.Value.FieldFunction((), name)) ==
      cm.Expr.FieldFunction(cm.ValueAttributes.empty, name))
  }

  "lowers an IfThenElse expression recursively" in {
    val in  = v3Value.Value.IfThenElse((), v3Value.Value.Unit(()), v3Value.Value.Unit(()), v3Value.Value.Unit(()))
    val out = cm.Expr.IfThenElse(
      cm.ValueAttributes.empty,
      cm.Expr.Unit(cm.ValueAttributes.empty),
      cm.Expr.Unit(cm.ValueAttributes.empty),
      cm.Expr.Unit(cm.ValueAttributes.empty)
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a Lambda expression, lowering the pattern" in {
    val in  = v3Value.Value.Lambda((), v3Value.wildcardPattern, v3Value.Value.Unit(()))
    val out = cm.Expr.Lambda(
      cm.ValueAttributes.empty,
      cm.Pattern.WildcardPattern(cm.ValueAttributes.empty),
      cm.Expr.Unit(cm.ValueAttributes.empty)
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a Destructure expression" in {
    val in  = v3Value.Value.Destructure((), v3Value.wildcardPattern, v3Value.Value.Unit(()), v3Value.Value.Unit(()))
    val out = cm.Expr.Destructure(
      cm.ValueAttributes.empty,
      cm.Pattern.WildcardPattern(cm.ValueAttributes.empty),
      cm.Expr.Unit(cm.ValueAttributes.empty),
      cm.Expr.Unit(cm.ValueAttributes.empty)
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a PatternMatch expression's cases" in {
    val in =
      v3Value.Value.PatternMatch((), v3Value.Value.Unit(()), ZChunk((v3Value.wildcardPattern, v3Value.Value.Unit(()))))
    val out = cm.Expr.PatternMatch(
      cm.ValueAttributes.empty,
      cm.Expr.Unit(cm.ValueAttributes.empty),
      Chunk(cm.MatchCase(cm.Pattern.WildcardPattern(cm.ValueAttributes.empty), cm.Expr.Unit(cm.ValueAttributes.empty)))
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers an UpdateRecord expression's field updates" in {
    val name = Name.fromString("field1")
    val in   = v3Value.Value.UpdateRecord((), v3Value.Value.Unit(()), Map(name -> v3Value.Value.Unit(())))
    val out  = cm.Expr.UpdateRecord(
      cm.ValueAttributes.empty,
      cm.Expr.Unit(cm.ValueAttributes.empty),
      Chunk(cm.RecordField(name, cm.Expr.Unit(cm.ValueAttributes.empty)))
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a LetDefinition expression" in {
    val name = Name.fromString("x")
    val defn = v3Value.Value.Definition(v3.Type.Unit(()), v3Value.Value.Unit(()))
    val in   = v3Value.Value.LetDefinition((), name, defn, v3Value.Value.Unit(()))
    val out  = cm.Expr.LetDefinition(
      cm.ValueAttributes.empty,
      name,
      cm.ValueDefinitionBody.ExpressionBody(
        Chunk.empty,
        cm.Type.Unit(cm.TypeAttributes.empty),
        cm.Expr.Unit(cm.ValueAttributes.empty)
      ),
      cm.Expr.Unit(cm.ValueAttributes.empty)
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "lowers a LetRecursion expression's bindings" in {
    val name = Name.fromString("x")
    val defn = v3Value.Value.Definition(v3.Type.Unit(()), v3Value.Value.Unit(()))
    val in   = v3Value.Value.LetRecursion((), Map(name -> defn), v3Value.Value.Unit(()))
    val out  = cm.Expr.LetRecursion(
      cm.ValueAttributes.empty,
      Chunk(
        cm.Binding(
          name,
          cm.ValueDefinitionBody
            .ExpressionBody(Chunk.empty, cm.Type.Unit(cm.TypeAttributes.empty), cm.Expr.Unit(cm.ValueAttributes.empty))
        )
      ),
      cm.Expr.Unit(cm.ValueAttributes.empty)
    )
    assert(V3Lowering.lowerExpr(in) == out)
  }

  "populates inferredType when lowering a TypedValue" in {
    val in: v3Value.TypedValue = v3Value.Value.Unit(v3.Type.Unit(()))
    assert(V3Lowering.lowerExpr(in) ==
      cm.Expr.Unit(cm.ValueAttributes.empty.copy(inferredType = Some(cm.Type.Unit(cm.TypeAttributes.empty)))))
  }

  "leaves inferredType empty when lowering a RawValue" in {
    val in: v3Value.RawValue = v3Value.Value.Unit(())
    assert(V3Lowering.lowerExpr(in) == cm.Expr.Unit(cm.ValueAttributes.empty))
  }

  // -------------------------------------------------------------------------------------------------------------
  // Value definitions / specifications
  // -------------------------------------------------------------------------------------------------------------

  "lowers a value specification's inputs and output" in {
    val name = Name.fromString("x")
    val in   = v3Value.Specification.Raw((name.toCamelCase, v3.unit))(v3.unit)
    val out  = cm.ValueSpecification(
      Chunk(cm.Parameter(name, cm.Type.Unit(cm.TypeAttributes.empty))),
      cm.Type.Unit(cm.TypeAttributes.empty)
    )
    assert(V3Lowering.lowerValueSpecification(in) == out)
  }

  // -------------------------------------------------------------------------------------------------------------
  // Distributions
  // -------------------------------------------------------------------------------------------------------------

  "lowers a Distribution.Library, preserving the package name and module count" in {
    val packageName = PackageName.fromString("My.Package")
    val moduleA     = ModuleName.fromString("ModuleA")
    val moduleB     = ModuleName.fromString("ModuleB")
    val packageDef  = V3PackageDefinition.Typed(
      Map(
        moduleA -> AccessControlled.publicAccess(v3Module.Definition.empty),
        moduleB -> AccessControlled.publicAccess(v3Module.Definition.empty)
      )
    )
    val library = V3Distribution.Library(packageName, Map.empty, packageDef)

    V3Lowering.lowerDistribution(library) match
      case cm.Distribution.Library(lib) =>
        assert(lib.packageInfo.name == packageName)
        assert(lib.definition.modules.size == 2)
      case other => assert(false)
  }

end V3LoweringSpec
