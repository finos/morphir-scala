package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.naming.FQName.getLocalName
import org.finos.morphir.naming.Name.toTitleCase
import org.finos.morphir.ir.PackageModule.{Definition => PackageDefinition, USpecification => UPackageSpecification}
import org.finos.morphir.ir.Value.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Value.{Pattern, Value}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.*
import org.finos.morphir.ir.{Module, Type}
import org.finos.morphir.runtime.{NativeSDK, RTValue, SDKConstructor, SDKValue}
import org.finos.morphir.runtime.internal.{CallStackFrame, StoredValue}
import org.finos.morphir.runtime.Distributions
import zio.Chunk

final case class Store(
    callStack: CallStackFrame
) {
  def getVariable(name: Name): Option[StoredValue] = callStack.get(name)
  def push(bindings: Map[Name, StoredValue])       = Store(callStack.push(bindings))
}

object Store {
  def empty = Store(CallStackFrame(Map(), None))
}

final case class GlobalDefs(
    definitions: Map[FQName, SDKValue],
    ctors: Map[FQName, SDKConstructor]
) {
  def getDefinition(name: FQName): Option[SDKValue] = definitions.get(name)
  def withBindingsFrom(other: GlobalDefs) =
    GlobalDefs(definitions ++ other.definitions, ctors ++ other.ctors)
  def withDefinition(fqn: FQName, definition: SDKValue): GlobalDefs =
    GlobalDefs(definitions + (fqn -> definition), ctors)
  def withConstructor(fqn: FQName, constructor: SDKConstructor): GlobalDefs =
    GlobalDefs(definitions, ctors + (fqn -> constructor))
  def getCtor(name: FQName): Option[SDKConstructor] = ctors.get(name)
}

object GlobalDefs {
  def fromDistributions(dists: Distribution*): GlobalDefs = {
    val libs: Map[PackageName, Lib] = Distribution.toLibsMap(dists: _*)
    libs.foldLeft(native) {
      case (acc, (packageName, lib)) => createDefs(acc, packageName, lib.dependencies, lib.packageDef)
    }
  }

  def fromDistributions(dists: Distributions): GlobalDefs = {
    val libs: Map[PackageName, Lib] = dists.getDists
    libs.foldLeft(native) {
      case (acc, (packageName, lib)) => createDefs(acc, packageName, lib.dependencies, lib.packageDef)
    }
  }

  def createDefs(
      acc: GlobalDefs,
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ): GlobalDefs =
    packageDef.modules.foldLeft(acc) { case (acc, (moduleName, module)) =>
      val withDefinitions = module.value.values.foldLeft(acc) { case (acc, (valueName, value)) =>
        val name       = FQName(packageName, moduleName, valueName)
        val definition = value.value.value
        val sdkDef     = SDKValue.SDKValueDefinition(definition)
        acc.withDefinition(name, sdkDef)
      }
      module.value.types.foldLeft(withDefinitions) { case (acc, (localName, tpe)) =>
        val typeDef = tpe.value.value
        typeDef match {
          case Type.Definition.CustomType(_, accessControlledCtors) =>
            val ctors = accessControlledCtors.value.toMap
            ctors.foldLeft(acc) { case (acc, (ctorName, ctorArgs)) =>
              val fqn = FQName(packageName, moduleName, ctorName)
              acc.withConstructor(fqn, SDKConstructor.Explicit(ctorArgs.map(_._2).toList))
            }
          case Type.Definition.TypeAlias(_, Type.Type.Record(_, fields)) =>
            val fqn = FQName(packageName, moduleName, localName)
            acc.withConstructor(fqn, SDKConstructor.Implicit(fields))
          case Type.Definition.TypeAlias(_, _) => acc
        }
      }
    }

  def empty: GlobalDefs =
    GlobalDefs(Map(), Map())
  def native: GlobalDefs =
    GlobalDefs(Native.native ++ NativeSDK.resolvedFunctions, Native.nativeCtors ++ NativeSDK.ctors)

  def getStaticallyReachable(ref: FQName, globalDefs: GlobalDefs): Set[FQName] = {

    def exploreBelow(currentlyKnown: Set[FQName], toExplore: FQName): Set[FQName] =
      if (currentlyKnown.contains(toExplore)) currentlyKnown
      else
        globalDefs.definitions.get(toExplore) match {
          case Some(SDKValue.SDKValueDefinition(x)) =>
            x.body.collectReferences.foldLeft(currentlyKnown + toExplore)((acc, next) => exploreBelow(acc, next))
          case _ => currentlyKnown
        }

    exploreBelow(Set(), ref)
  }
}
