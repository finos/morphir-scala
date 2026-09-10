package org.finos.morphir.datamodel.classic.codecs

1.18.0import io.bullet.borer._
1.18.0import io.bullet.borer.derivation.MapBasedCodecs._
import org.finos.morphir._
import org.finos.morphir.datamodel.classic._
import org.finos.morphir.datamodel.classic.codecs.MdmCodecs._

object Utils {
  def testCodec(data: Data): Boolean = {
    val bytes = Cbor.encode[Data](data).toByteArray
    Cbor.decode(bytes).to[Data].value == data
  }
}
