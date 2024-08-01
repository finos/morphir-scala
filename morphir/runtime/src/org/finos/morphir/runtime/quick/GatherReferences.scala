package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Value.{Pattern, Value => V}
import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.Value.Pattern.*
import org.finos.morphir.ir.Value.{Value, TypedValue}
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.distribution.Distribution.Lib
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.runtime.SDKValue

import scala.collection.immutable.Set
import zio.Chunk

object GatherReferences {
  // Gather references from distribution*
  // Also from GlobalDefs? (Yeah, redundancy is okay, and there are
  // Helper: Recursively explore value
  // Diff vs. GlobalDefs
  def fromGlobalDefs(globals: GlobalDefs): ReferenceSet =
    globals.definitions.map { case (name, value) =>
      value match {
        case SDKValue.SDKValueDefinition(definition) => loop(definition.body).withDefinition(name) // TODO: Types!
        case _                                       => ReferenceSet.empty.withDefinition(name)
      }
    }.foldLeft(ReferenceSet.empty)((acc, next) => acc ++ next) ++
      globals.ctors.keys.foldLeft(ReferenceSet.empty)((acc, next) => acc.withConstructor(next))

  def fromEntrySet(entrySet: ReferenceSet, dists: Distribution*): ReferenceSet = {
    val mapped = Distribution.toLibsMapUnsafe(dists: _*)

    def f(known: Set[FQName], ref: FQName): Set[FQName] = {
      // if (depth > 100) throw new Exception(s"Still recursing on $next with known values ${known.toList.mkString("\n")}")
      val (pkg, mod, loc) = (ref.pack, ref.getModulePath, ref.localName)
      val qName           = QName(mod, loc)
      mapped.get(pkg) match {
        case Some(lib) =>
          val definition = lib.lookupValueDefinition(qName).get
          val discovered = loop(definition.body).definitions
          val newbs      = discovered.diff(known)
          println(s"Exploring $ref found $discovered")
          newbs.foldLeft(known ++ discovered)((acc, newb) => acc ++ f(acc, newb))
        case None => known + ref
      }
    }
    val found = entrySet.definitions.foldLeft(entrySet.definitions)((acc, newb) => acc ++ f(acc, newb))
    ReferenceSet(entrySet.definitions ++ found, Set(), Set())
  }

  def fromDistributions(dists: Distribution*): ReferenceSet =
    fromDistributionLibs(Distribution.toLibsMapUnsafe(dists: _*))

  def fromDistributionLibs(libs: Map[PackageName, Lib]): ReferenceSet =
    libs.foldLeft(ReferenceSet.empty) { case (acc: ReferenceSet, (packageName: PackageName, lib: Lib)) =>
      acc ++ fromLib(packageName, lib)
    }

  def fromLib(packageName: PackageName, lib: Lib): ReferenceSet = {
    val valueReferences: List[ReferenceSet] =
      lib.packageDef.modules.toList.flatMap { case (moduleName, accessControlledModule) =>
        accessControlledModule.value.values.map {
          case (localName, accessControlledValue) =>
            val fqn        = FQName(packageName, moduleName, localName)
            val definition = accessControlledValue.value.value
            loop(definition.body).withDefinition(fqn)
        }
      }
    val ctorReferences: List[ReferenceSet] =
      lib.packageDef.modules.toList.flatMap { case (moduleName, accessControlledModule) =>
        accessControlledModule.value.types.flatMap {
          case (_, accessControlledType) =>
            val definition = accessControlledType.value.value
            definition match {
              case T.Definition.CustomType(_, accessControlledCtors) =>
                val ctors = accessControlledCtors.value.toMap
                ctors.map { case (ctorName, _) =>
                  val name = FQName(packageName, moduleName, ctorName)
                  ReferenceSet.empty.withConstructor(name)
                }
              case T.Definition.TypeAlias(_, _) => List(ReferenceSet.empty)
            }
        }
      }
    valueReferences.foldLeft(ReferenceSet.empty)((acc, next) => acc ++ next) ++
      ctorReferences.foldLeft(ReferenceSet.empty)((acc, next) => acc ++ next)
  }

  case class ReferenceSet(
      definitions: Set[FQName],
      constructors: Set[FQName],
      types: Set[FQName]
  ) {
    def ++(other: ReferenceSet): ReferenceSet =
      ReferenceSet(
        definitions ++ other.definitions,
        constructors ++ other.constructors,
        types ++ other.types
      )
    def withConstructor(ctor: FQName) =
      ReferenceSet(
        definitions,
        constructors + ctor,
        types
      )

    def withDefinition(definition: FQName) =
      ReferenceSet(
        definitions + definition,
        constructors,
        types
      )
  }
  object ReferenceSet {
    val empty = ReferenceSet(Set(), Set(), Set())
  }

  def loop(ir: TypedValue): ReferenceSet = {
    val empty = ReferenceSet.empty;

    def fold(stuff: Iterable[TypedValue]): ReferenceSet = stuff.foldLeft(empty) { case (acc, next) =>
      acc ++ loop(next)
    }

    ir match {
      case Value.Literal(_, _)                => empty
      case Value.Apply(_, function, argument) => loop(function) ++ loop(argument)
      case Value.Destructure(_, pattern, valueToDestruct, inValue) =>
        loop(valueToDestruct) ++ loop(inValue) ++ patternLoop(pattern)
      case Value.Constructor(_, fqn)      => empty.withConstructor(fqn)
      case Value.Field(_, recordValue, _) => loop(recordValue)
      case Value.FieldFunction(_, _)      => empty
      case Value.IfThenElse(_, condition, thenValue, elseValue) =>
        loop(condition) ++ loop(thenValue) ++ loop(elseValue)
      case Value.Lambda(_, pattern, body) => patternLoop(pattern) ++ loop(body)
      case Value.LetDefinition(_, _, definition, inValue) =>
        loop(definition.body) ++ loop(inValue)
      case Value.LetRecursion(_, definitions, inValue) => fold(definitions.map(_._2.body)) ++ loop(inValue)
      case Value.List(_, elements)                     => fold(elements)
      case Value.PatternMatch(_, value, cases) => loop(value) ++ cases.foldLeft(empty) { case (acc, (pattern, value)) =>
          acc ++ loop(value) ++ patternLoop(pattern)
        }
      case Value.Record(_, fields)                      => fold(fields.map(_._2))
      case Value.Reference(_, name)                     => empty.withDefinition(name)
      case Value.Tuple(_, elements)                     => fold(elements)
      case Value.Unit(_)                                => empty
      case Value.UpdateRecord(_, valueToUpdate, fields) => loop(valueToUpdate) ++ fold(fields.map(_._2))
      case Value.Variable(_, _)                         => empty
    }
  }
  def patternLoop(pattern: Pattern[UType]): ReferenceSet = {
    val empty = ReferenceSet.empty;
    def fold(stuff: Chunk[Pattern[UType]]): ReferenceSet = stuff.foldLeft(empty) { case (acc, next) =>
      acc ++ patternLoop(next)
    }
    pattern match {
      case _: WildcardPattern[_]                        => empty
      case AsPattern(_, innerPattern, _)                => patternLoop(innerPattern)
      case _: UnitPattern[_]                            => empty
      case LiteralPattern(_, _)                         => empty
      case _: EmptyListPattern[_]                       => empty
      case HeadTailPattern(_, headPattern, tailPattern) => patternLoop(headPattern) ++ patternLoop(tailPattern)
      case TuplePattern(_, patterns)                    => fold(patterns)
      case ConstructorPattern(_, patternName, patterns) => empty.withConstructor(patternName) ++ fold(patterns)
    }
  }
}
