package org.finos.morphir.ir.module

trait ModuleSpecFor[A] {

  def module: QualifiedModuleName
  def spec: Specification[Any]
}

object ModuleSpecFor {

  /** Summon the module specification for the given module/type. */
  def apply[A](implicit specFor: ModuleSpecFor[A]): ModuleSpecFor[A] = specFor

  def make[A](name: QualifiedModuleName)(moduleSpec: Specification[Any]): ModuleSpecFor[A] =
    new ModuleSpecFor[A] {
      val module = name
      val spec   = moduleSpec
    }
}
