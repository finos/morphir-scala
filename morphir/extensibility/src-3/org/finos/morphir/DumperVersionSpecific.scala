package org.finos.morphir

import Dumper.*
trait DumperVersionSpecific:
  given [A](using Dumper[A]): Dumper[IArray[A]] with
    def dumper(array: IArray[A]): Repr =
      Repr.VConstructor(List("scala"), "IArray", array.toList.map(_.dumperRepr))
