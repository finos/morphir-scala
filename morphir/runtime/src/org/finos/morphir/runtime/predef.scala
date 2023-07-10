package org.finos.morphir.runtime

import zio.prelude._
object predef {
  type Environment = Environment.Type
  object Environment extends Newtype[CallStack]
}
