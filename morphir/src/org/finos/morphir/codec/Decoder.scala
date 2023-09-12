package org.finos.morphir.codec

import zio.stream.ZPipeline
trait Decoder[Whole, Element, +A] {}
