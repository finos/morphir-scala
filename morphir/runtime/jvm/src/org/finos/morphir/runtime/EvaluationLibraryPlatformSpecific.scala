package org.finos.morphir.runtime

import org.finos.morphir.ir.Value.TypedValue
import org.finos.morphir.ir.Value as V
import V.*
import V.Value.{List as ListValue, Unit as UnitValue, *}
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.{FQName, Module, Name, QName, Type}
import org.finos.morphir.ir.distribution.Distribution.Library
import org.finos.morphir.ir.MorphirIRFile

import scala.io.Source
import zio.json.*
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.runtime.quick.{EvaluatorQuick, Store}

trait EvaluationLibraryPlatformSpecific {
  def apply(fileName: String, prefix: Option[String] = None): EvaluationLibrary = {
    val text = Source
      .fromFile(fileName)
      .getLines()
      .mkString("\n")
    val morphirIRFile = text.fromJson[MorphirIRFile]
    val library = morphirIRFile
      .getOrElse(throw new Exception(morphirIRFile.toString))
      .distribution
      .asInstanceOf[Library]
    val store = Store.fromLibrary(library)
    EvaluationLibrary(store, prefix, library)
  }

  def apply(fileName: String, prefix: String): EvaluationLibrary = apply(fileName, Some(prefix))
}
