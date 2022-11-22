package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Type.Constructors
import org.finos.morphir.ir.Type.Type
import zio._
import zio.test.Gen

trait ConstructorsGen {
  final def constructorsFromMap[R, A](toMapGen: Gen[R, Map[Name, Chunk[(Name, Type[A])]]]): Gen[R, Constructors[A]] =
    for {
      toMap <- toMapGen
    } yield Constructors(toMap)

  final def constructorsFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Constructors[A]] =
    constructorsFromMap(toMapGen)

  private final def toMapGen[R, A](implicit attributes: Gen[R, A]): Gen[R, Map[Name, Chunk[(Name, Type[A])]]] =
    Gen.mapOfBounded(0, 2)(NameGen.name, nameToTypeChunkGen)

  private final def nameToTypeChunkGen[R, A](implicit attributes: Gen[R, A]): Gen[R, Chunk[(Name, Type[A])]] =
    Gen.chunkOfBounded(1, 2)((NameGen.name <*> TypeGen.typeGen))
}

object ConstructorsGen extends ConstructorsGen
