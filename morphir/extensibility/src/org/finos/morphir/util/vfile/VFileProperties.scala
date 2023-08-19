package org.finos.morphir.util.vfile
import org.finos.morphir.util.props.Property

import java.nio.charset.{Charset, StandardCharsets}
object VFileProperties {

  val isEmpty: Property[Boolean] =
    Property("isEmpty", false)

  val characterSet: Property[Charset] =
    Property("characterSet", StandardCharsets.UTF_8)

}
