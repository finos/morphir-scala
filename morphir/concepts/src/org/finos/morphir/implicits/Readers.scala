package org.finos
package morphir
package implicits

import morphir.core.*
import org.finos.morphir.core.types.Naming.Name
trait Readers extends morphir.core.Types:
  given NameReader: Reader[Name] = new SimpleReader[Name] {
    override def expectedMsg: String = "expected Name"

    override def visitName(value: String, index: Int): Name = Name(value)
  }
