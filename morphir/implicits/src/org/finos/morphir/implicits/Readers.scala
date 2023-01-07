package org.finos
package morphir
package implicits

import morphir.core.*
trait Readers extends morphir.core.Types:
  given NameReader: Reader[Name] = new SimpleReader[Name] {
    override def expectedMsg: String = "expected Name"

    override def visitName(value: CharSequence, index: Int): Name = Name(value)
  }
