package org.finos.morphir.extensibility
import org.finos.morphir.naming._
import scala.collection.mutable.{IndexedSeq => MutIndexedSeq}

sealed trait MorphirModule {
  def packageName: String
  def moduleName: String
}

abstract class UserModule(val packageName: String, val moduleName: String) extends MorphirModule {}

sealed abstract class NativeModule(val packageName: String, val moduleName: String) extends MorphirModule {}

abstract class SdkModule(packageName: String, moduleName: String)
    extends NativeModule(packageName, moduleName) {}
