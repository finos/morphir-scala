package org.finos.morphir.util.vfile
import org.finos.morphir.util.attribs.Attribute

import java.nio.charset.{Charset, StandardCharsets}
object VFileProperties {

  val isEmpty: Attribute[Boolean] =
    Attribute("isEmpty", false)

  val characterSet: Attribute[Charset] =
    Attribute("characterSet", StandardCharsets.UTF_8)

}
