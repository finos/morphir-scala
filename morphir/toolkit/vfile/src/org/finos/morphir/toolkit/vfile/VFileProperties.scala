package org.finos.morphir.toolkit.vfile
import org.finos.morphir.toolkit.props.Property
import zio.EnvironmentTag

import java.nio.charset.{Charset, StandardCharsets}
object VFileProperties {

  val isEmpty:Property[Boolean] =
    Property("isEmpty", false)

  val characterSet:Property[Charset] =
    Property("characterSet", StandardCharsets.UTF_8)

}
