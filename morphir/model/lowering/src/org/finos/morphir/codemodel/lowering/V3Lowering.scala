package org.finos.morphir.codemodel.lowering

import kyo.Chunk
import org.finos.morphir.naming.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.ir.AccessControlled
import org.finos.morphir.ir.Documented
import org.finos.morphir.ir.Type as v3
import org.finos.morphir.ir.Value as v3Value
import org.finos.morphir.ir.Literal as v3Literal
import org.finos.morphir.ir.Module as v3Module
import org.finos.morphir.ir.packages.{Definition as V3PackageDefinition, Specification as V3PackageSpecification}
import org.finos.morphir.ir.distribution.Distribution as V3Distribution

/**
 * Lowers the v3 IR (`org.finos.morphir.ir`, `morphir-ir.json`) to the code model (`org.finos.morphir.codemodel`, the v4
 * IR).
 *
 * v3's `Type[+A]` and `Value[+TA, +VA]` are polymorphic over attributes; the code model's `TypeAttributes` and
 * `ValueAttributes` are fixed records. Attributes are therefore erased, not translated: lowering a `TypedValue` (whose
 * value-level attribute is a `UType`) populates `ValueAttributes.inferredType`; lowering anything else (a `RawValue`,
 * or a type-level attribute, which is always `Unit` in this codebase) produces `empty`. See `valueAttributesOf` below
 * for the mechanism.
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

  def lowerTypeDefinition(d: v3.Definition[Any]): cm.TypeDefinition =
    d match
      case v3.Definition.TypeAlias(params, typeExp) =>
        cm.TypeDefinition.TypeAliasDefinition(Chunk.from(params), lowerType(typeExp))
      case v3.Definition.CustomType(params, access) =>
        cm.TypeDefinition.CustomTypeDefinition(
          Chunk.from(params),
          cm.AccessControlled(lowerAccess(access.access), lowerConstructors(access.value))
        )

  def lowerTypeSpecification(s: v3.Specification[Any]): cm.TypeSpecification =
    s match
      case v3.Specification.TypeAliasSpecification(params, expr) =>
        cm.TypeSpecification.TypeAliasSpecification(Chunk.from(params), lowerType(expr))
      case v3.Specification.OpaqueTypeSpecification(params) =>
        cm.TypeSpecification.OpaqueTypeSpecification(Chunk.from(params))
      case v3.Specification.CustomTypeSpecification(params, ctors) =>
        cm.TypeSpecification.CustomTypeSpecification(Chunk.from(params), lowerConstructors(ctors))

  /**
   * v3 stores a type's constructors as a `Map[Name, Chunk[(Name, Type)]]` (`Constructors.toMap`), which does not
   * preserve declaration order for maps beyond a handful of entries. The code model wants a `Chunk[Constructor]`; we
   * iterate the map in whatever order it hands back, same as v3 itself does everywhere it folds over `Constructors`.
   * This is a pre-existing v3 property, not something introduced by lowering.
   */
  def lowerConstructors(ctors: v3.Constructors[Any]): Chunk[cm.Constructor] =
    Chunk.from(ctors.toMap.toSeq.map { case (name, args) =>
      cm.Constructor(
        name,
        Chunk.from(args.map { case (paramName, paramType) => cm.Parameter(paramName, lowerType(paramType)) })
      )
    })

  // ---------------------------------------------------------------------------------------------------------------
  // Literals
  // ---------------------------------------------------------------------------------------------------------------

  def lowerLiteral(l: v3Literal.Lit): cm.Literal =
    l match
      case v3Literal.Literal.BoolLiteral(value)        => cm.Literal.BoolLiteral(value)
      case v3Literal.Literal.CharLiteral(value)        => cm.Literal.CharLiteral(value.toString)
      case v3Literal.Literal.StringLiteral(value)      => cm.Literal.StringLiteral(value)
      case v3Literal.Literal.WholeNumberLiteral(value) => cm.Literal.IntegerLiteral(BigInt(value))
      case v3Literal.Literal.FloatLiteral(value)       => cm.Literal.FloatLiteral(value)
      case v3Literal.Literal.DecimalLiteral(value)     => cm.Literal.DecimalLiteral(value)

  // ---------------------------------------------------------------------------------------------------------------
  // Patterns
  // ---------------------------------------------------------------------------------------------------------------

  def lowerPattern(p: v3Value.Pattern[Any]): cm.Pattern =
    val attrs = valueAttributesOf(p.attributes)
    p match
      case v3Value.Pattern.WildcardPattern(_)        => cm.Pattern.WildcardPattern(attrs)
      case v3Value.Pattern.AsPattern(_, inner, name) => cm.Pattern.AsPattern(attrs, lowerPattern(inner), name)
      case v3Value.Pattern.TuplePattern(_, elements) =>
        cm.Pattern.TuplePattern(attrs, Chunk.from(elements.map(lowerPattern)))
      case v3Value.Pattern.ConstructorPattern(_, ctorName, args) =>
        cm.Pattern.ConstructorPattern(attrs, ctorName, Chunk.from(args.map(lowerPattern)))
      case v3Value.Pattern.EmptyListPattern(_)            => cm.Pattern.EmptyListPattern(attrs)
      case v3Value.Pattern.HeadTailPattern(_, head, tail) =>
        cm.Pattern.HeadTailPattern(attrs, lowerPattern(head), lowerPattern(tail))
      case v3Value.Pattern.LiteralPattern(_, literal) => cm.Pattern.LiteralPattern(attrs, lowerLiteral(literal))
      case v3Value.Pattern.UnitPattern(_)             => cm.Pattern.UnitPattern(attrs)

  // ---------------------------------------------------------------------------------------------------------------
  // Expressions
  // ---------------------------------------------------------------------------------------------------------------

  /**
   * v3's value-level attribute (`VA`) is polymorphic: it is `Unit` for a `RawValue` and `UType` for a `TypedValue`.
   * Since both are erased to `Any` once bound to `Value[Any, Any]`, we recover the distinction with a runtime type test
   * rather than two overloads (which would clash after erasure, since `RawValue` and `TypedValue` both erase to
   * `Value`). Anything that isn't itself a v3 `Type` (i.e. a `Unit` attribute, or any other non-type attribute that
   * never occurs in this codebase) erases to `ValueAttributes.empty`.
   */
  private def valueAttributesOf(va: Any): cm.ValueAttributes =
    va match
      case tpe: v3.Type[Any] @unchecked => cm.ValueAttributes.empty.copy(inferredType = Some(lowerType(tpe)))
      case _                            => cm.ValueAttributes.empty

  def lowerExpr(v: v3Value.Value[Any, Any]): cm.Expr =
    val attrs = valueAttributesOf(v.attributes)
    v match
      case v3Value.Value.Literal(_, literal) => cm.Expr.Literal(attrs, lowerLiteral(literal))
      case v3Value.Value.Constructor(_, fq)  => cm.Expr.Constructor(attrs, fq)
      case v3Value.Value.Tuple(_, elements)  => cm.Expr.Tuple(attrs, Chunk.from(elements.map(lowerExpr)))
      case v3Value.Value.List(_, elements)   => cm.Expr.List(attrs, Chunk.from(elements.map(lowerExpr)))
      case v3Value.Value.Record(_, fields)   =>
        cm.Expr.Record(
          attrs,
          Chunk.from(fields.map { case (name, fieldValue) => cm.RecordField(name, lowerExpr(fieldValue)) })
        )
      case v3Value.Value.Unit(_)                      => cm.Expr.Unit(attrs)
      case v3Value.Value.Variable(_, name)            => cm.Expr.Variable(attrs, name)
      case v3Value.Value.Reference(_, fq)             => cm.Expr.Reference(attrs, fq)
      case v3Value.Value.Field(_, subject, name)      => cm.Expr.Field(attrs, lowerExpr(subject), name)
      case v3Value.Value.FieldFunction(_, name)       => cm.Expr.FieldFunction(attrs, name)
      case v3Value.Value.Apply(_, function, argument) =>
        cm.Expr.Apply(attrs, lowerExpr(function), lowerExpr(argument))
      case v3Value.Value.Lambda(_, argumentPattern, body) =>
        cm.Expr.Lambda(attrs, lowerPattern(argumentPattern), lowerExpr(body))
      case v3Value.Value.LetDefinition(_, name, definition, inValue) =>
        cm.Expr.LetDefinition(attrs, name, lowerValueDefinitionBody(definition), lowerExpr(inValue))
      case v3Value.Value.LetRecursion(_, definitions, inValue) =>
        cm.Expr.LetRecursion(
          attrs,
          Chunk.from(definitions.map { case (name, definition) =>
            cm.Binding(name, lowerValueDefinitionBody(definition))
          }),
          lowerExpr(inValue)
        )
      case v3Value.Value.Destructure(_, pattern, valueToDestruct, inValue) =>
        cm.Expr.Destructure(attrs, lowerPattern(pattern), lowerExpr(valueToDestruct), lowerExpr(inValue))
      case v3Value.Value.IfThenElse(_, condition, thenBranch, elseBranch) =>
        cm.Expr.IfThenElse(attrs, lowerExpr(condition), lowerExpr(thenBranch), lowerExpr(elseBranch))
      case v3Value.Value.PatternMatch(_, branchOutOn, cases) =>
        cm.Expr.PatternMatch(
          attrs,
          lowerExpr(branchOutOn),
          Chunk.from(cases.map { case (pattern, caseValue) =>
            cm.MatchCase(lowerPattern(pattern), lowerExpr(caseValue))
          })
        )
      case v3Value.Value.UpdateRecord(_, valueToUpdate, fieldsToUpdate) =>
        cm.Expr.UpdateRecord(
          attrs,
          lowerExpr(valueToUpdate),
          Chunk.from(fieldsToUpdate.map { case (name, fieldValue) => cm.RecordField(name, lowerExpr(fieldValue)) })
        )

  /**
   * v3's `ValueDefinition` always carries a concrete `body`: there is no v3 notion of a native, external or incomplete
   * value body, so it always lowers to `ExpressionBody`. `NativeBody`, `ExternalBody` and `IncompleteBody` have no v3
   * source and are never produced here.
   */
  def lowerValueDefinitionBody(d: v3Value.Definition[Any, Any]): cm.ValueDefinitionBody =
    cm.ValueDefinitionBody.ExpressionBody(
      inputTypes = Chunk.from(d.inputTypes.map { case (name, _, tpe) => cm.Parameter(name, lowerType(tpe)) }),
      outputType = lowerType(d.outputType),
      body = lowerExpr(d.body)
    )

  /**
   * The code model's `ValueDefinition` wraps its body in its own `AccessControlled`, independent of the
   * `AccessControlled` that a `ModuleDefinition`'s `values` map already carries per entry. v3 has only the one,
   * module-level access marker (`module.Definition.values: Map[Name, AccessControlled[Documented[ValueDefinition]]]`);
   * there's nothing in v3 to distinguish a second, body-level access. We carry the same access value into both wrapper
   * levels rather than inventing an independent one.
   */
  def lowerValueDefinition(access: AccessControlled.Access, d: v3Value.Definition[Any, Any]): cm.ValueDefinition =
    cm.ValueDefinition(cm.AccessControlled(lowerAccess(access), lowerValueDefinitionBody(d)))

  def lowerValueSpecification(spec: v3Value.Specification[Any]): cm.ValueSpecification =
    cm.ValueSpecification(
      inputs = Chunk.from(spec.inputs.map { case (name, tpe) => cm.Parameter(name, lowerType(tpe)) }),
      output = lowerType(spec.output)
    )

  // ---------------------------------------------------------------------------------------------------------------
  // Access / documentation
  // ---------------------------------------------------------------------------------------------------------------

  private def lowerAccess(a: AccessControlled.Access): cm.Access =
    a match
      case AccessControlled.Access.Public  => cm.Access.Public
      case AccessControlled.Access.Private => cm.Access.Private

  /**
   * v3's `Documented` always carries a (possibly-empty) `String`; the code model's carries an `Option[Documentation]`
   * where `Documentation` is a `Chunk` of lines. An empty v3 doc string lowers to `None`; anything else is split on
   * newlines into `Documentation`'s lines.
   */
  private def lowerDocumented[A, B](d: Documented[A])(f: A => B): cm.Documented[B] =
    val doc = if d.doc.isEmpty then None else Some(cm.Documentation(Chunk.from(d.doc.linesIterator.toSeq)))
    cm.Documented(doc, f(d.value))

  // ---------------------------------------------------------------------------------------------------------------
  // Modules / packages / distributions
  // ---------------------------------------------------------------------------------------------------------------

  def lowerModuleDefinition(d: v3Module.Definition[Any, Any]): cm.ModuleDefinition =
    cm.ModuleDefinition(
      types = d.types.map { case (name, access) =>
        name -> cm.AccessControlled(lowerAccess(access.access), lowerDocumented(access.value)(lowerTypeDefinition))
      },
      values = d.values.map { case (name, access) =>
        name -> cm.AccessControlled(
          lowerAccess(access.access),
          lowerDocumented(access.value)(vd => lowerValueDefinition(access.access, vd))
        )
      }
    )

  def lowerModuleSpecification(s: v3Module.Specification[Any]): cm.ModuleSpecification =
    cm.ModuleSpecification(
      types = s.types.map { case (name, doc) => name -> lowerDocumented(doc)(lowerTypeSpecification) },
      values = s.values.map { case (name, doc) => name -> lowerDocumented(doc)(lowerValueSpecification) }
    )

  def lowerPackageDefinition(d: V3PackageDefinition[Any, Any]): cm.PackageDefinition =
    cm.PackageDefinition(
      modules = d.modules.map { case (name, access) =>
        name -> cm.AccessControlled(lowerAccess(access.access), lowerModuleDefinition(access.value))
      }
    )

  def lowerPackageSpecification(s: V3PackageSpecification[Any]): cm.PackageSpecification =
    cm.PackageSpecification(
      modules = s.modules.map { case (name, moduleSpec) => name -> lowerModuleSpecification(moduleSpec) }
    )

  /**
   * Only `Distribution.Library` has a code-model counterpart. `Distribution.Bundle` is a `Map[PackageName, Lib]` — a
   * collection of libraries with no single package name or definition of its own — and none of `cm.Distribution`'s
   * cases (`Library`, `Specs`, `Application`) model a multi-package collection. Lowering a `Bundle` would require
   * inventing a mapping the code model doesn't have; per the task's ground rule ("if a v3 case has no clean mapping,
   * stop and report rather than inventing one") this throws instead. Callers with a `Bundle` should lower each of its
   * `toLibraries`/`allDistributions` individually.
   */
  def lowerDistribution(d: V3Distribution): cm.Distribution =
    d match
      case lib: V3Distribution.Library =>
        cm.Distribution.Library(
          cm.LibraryDistribution(
            packageInfo = cm.PackageInfo(lib.packageName, ""),
            definition = lowerPackageDefinition(lib.packageDef),
            dependencies = lib.dependencies.map { case (packageName, spec) =>
              packageName -> lowerPackageSpecification(spec)
            }
          )
        )
      case _: V3Distribution.Bundle =>
        throw new UnsupportedOperationException(
          "V3Lowering.lowerDistribution: v3 Distribution.Bundle has no code-model counterpart (cm.Distribution " +
            "models a single package - Library, Specs or Application; Bundle is a multi-package collection). " +
            "Lower each contained Library individually via Bundle#toLibraries."
        )
