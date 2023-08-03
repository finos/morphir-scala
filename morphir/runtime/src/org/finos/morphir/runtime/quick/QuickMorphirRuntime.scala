package org.finos.morphir.runtime.quick

import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value
import org.finos.morphir.ir.Value as V
import org.finos.morphir.datamodel.Data
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.Utils.*
import org.finos.morphir.ir.{QName, FQName}
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.Module.QualifiedModuleName
import org.finos.morphir.ir.PackageModule.PackageName

import scala.util.{Failure, Success, Try}
import org.finos.morphir.runtime.{EvaluationError, MorphirRuntimeError}

private[runtime] case class QuickMorphirRuntime(library: Library, store: Store[scala.Unit, UType])
    extends TypedMorphirRuntime {
  // private val store: Store[scala.Unit, UType] = Store.empty //

  def evaluate(entryPoint: FQName, params: Value[scala.Unit, UType]): RT[MorphirRuntimeError, Data] =
    for {
      tpe <- fetchType(entryPoint)
      res <- evaluate(Value.Reference.Typed(tpe, entryPoint), params)
    } yield res

  def evaluate(value: Value[scala.Unit, UType]): RT[EvaluationError, Data] =
    try
      RT.succeed(EvaluatorQuick.eval(value, store, library))
    catch {
      case e: EvaluationError => RT.fail(e)
    }

  def fetchType(ref: FQName): RT[MorphirRuntimeError, UType] = {
    val (pkg, mod, loc) = (ref.getPackagePath, ref.getModulePath, ref.localName)
    val qName           = QName.fromTuple(mod, loc)
    val maybeSpec       = library.lookupValueSpecification(PackageName(pkg), QualifiedModuleName.fromPath(mod), loc)
    maybeSpec match {
      case Some(spec) => RT.succeed(specificationToType(spec))
      case None       => RT.fail(new SpecificationNotFound(s"Could not find $ref during initial type building"))
    }
  }

}

object QuickMorphirRuntime {

  def fromDistribution(distribution: Distribution): QuickMorphirRuntime = {
    val library = distribution
      .asInstanceOf[Library]
    val store = Store.fromLibrary(library)
    QuickMorphirRuntime(library, store)
  }

}
