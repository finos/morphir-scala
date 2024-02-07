package org.finos.morphir.datamodel.codecs

import io.bullet.borer._
import io.bullet.borer.derivation.MapBasedCodecs._
import org.finos.morphir._
import java.time._

object BaseCodecs {
  implicit val localTimeEncoder: Encoder[LocalTime] = Encoder { (writer, time: LocalTime) =>
    writer.writeString(time.toString)
  }

  implicit val localTimeDecoder: Decoder[LocalTime] = Decoder { reader =>
    LocalTime.parse(reader.readString())
  }

  implicit val localDateEncoder: Encoder[LocalDate] = Encoder { (writer, date: LocalDate) =>
    writer.writeString(date.toString)
  }

  implicit val localDateDecoder: Decoder[LocalDate] = Decoder { reader =>
    LocalDate.parse(reader.readString())
  }

  implicit val monthEncoder: Encoder[Month] = Encoder { (writer, month: Month) =>
    writer.writeInt(month.getValue())
  }

  implicit val monthDecoder: Decoder[Month] = Decoder { reader =>
    Month.of(reader.readInt())
  }

  implicit val dayOfWeekEncoder: Encoder[DayOfWeek] = Encoder { (writer, day: DayOfWeek) =>
    writer.writeInt(day.getValue())
  }

  implicit val dayOfWeekDecoder: Decoder[DayOfWeek] = Decoder { reader =>
    DayOfWeek.of(reader.readInt())
  }
}
