package org.finos.morphir.datamodel.codecs

import io.bullet.borer._
import io.bullet.borer.derivation.MapBasedCodecs._
import org.finos.morphir._
import org.finos.morphir.naming._

object NamingCodecs {
  implicit val nameEncoder: Encoder[Name] = Encoder { (writer, name: Name) =>
    writer.writeString(name.mkString(n => n)(","))
  }

  implicit val nameDecoder: Decoder[Name] = Decoder { reader =>
    Name(reader.readString())
  }

  implicit val pathEncoder: Encoder[Path] = Encoder { (writer, path: Path) =>
    writer.writeString(path.toString(n => n.toString, ","))
  }

  implicit val pathDecoder: Decoder[Path] = Decoder { reader =>
    Path(reader.readString())
  }

  implicit val fqNameEncoder: Encoder[FQName] = Encoder { (writer, name: FQName) =>
    writer.writeString(name.toString)
  }

  implicit val fqNameDecoder: Decoder[FQName] = Decoder { reader =>
    FQName.fromString(reader.readString(), ":")(FQNamingOptions.default)
  }
}
