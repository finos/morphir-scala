// package org.finos.morphir.universe.ir

// import zio.prelude.*

// final case class PackageSpecification[+TA](modules: Map[ModuleName, Module.Specification[TA]]) {
//   def map[B](f: TA => B): PackageSpecification[B] = PackageSpecification(modules.map { case (k, v) => k -> v.map(f) })
// }

// object PackageSpecification {}
