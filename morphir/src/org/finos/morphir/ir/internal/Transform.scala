package org.finos.morphir.ir.internal

import zio.Chunk
import zio.prelude.fx.ZPure

trait Transform[T] {
  import Transform._

  protected def const[A](a: A): Stateful[T, A] = ZPure.succeed(a)

  protected def ofList[A](list: List[A])(mapping: A => Stateful[T, A]) =
    list.foldLeft(const(List[A]())) { (pureList, elem) =>
      for {
        list  <- pureList
        elem1 <- mapping(elem)
      } yield (list :+ elem1)
    }

  protected def ofChunk[A](list: Chunk[A])(mapping: A => Stateful[T, A]) =
    list.foldLeft(const(Chunk.empty[A])) { (pureList, elem) =>
      for {
        chunk <- pureList
        elem1 <- mapping(elem)
      } yield (chunk :+ elem1)
    }

  protected def ofMapValues[K, V](map: Map[K, V])(mapping: V => Stateful[T, V]) =
    map.foldLeft(const(Map[K, V]())) { (pureMap, elem) =>
      for {
        map   <- pureMap
        elem1 <- {
          val (k, v) = elem
          mapping(v).map(v1 => (k, v1))
        }
      } yield (map ++ Map(elem1))
    }
}

object Transform {
  type Stateful[T, A] = ZPure[Nothing, T, T, Any, Nothing, A]
}
