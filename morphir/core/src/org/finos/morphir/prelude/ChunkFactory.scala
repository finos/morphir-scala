/*
 * Copyright 2023 Morphir Contributors
 * Copyright 2018-2023 John A. De Goes and the ZIO Contributors
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package org.finos.morphir.prelude

/**
 * Ported from ZIO: https://github.com/zio/zio/blob/series/2.x/core/shared/src/main/scala-2.13%2B/zio/ChunkFactory.scala
 */

// import zio.stacktracer.TracingImplicits.disableAutoTrace

import scala.collection.{IterableOnce, StrictOptimizedSeqFactory}
import scala.collection.mutable.Builder

private[prelude] trait ChunkFactory extends StrictOptimizedSeqFactory[Chunk] {

  final def from[A](source: IterableOnce[A]): Chunk[A] =
    source match {
      case iterable: Iterable[A] => Chunk.fromIterable(iterable)
      case iterableOnce =>
        val chunkBuilder = ChunkBuilder.make[A]()
        iterableOnce.iterator.foreach(chunkBuilder.addOne)
        chunkBuilder.result()
    }
}
