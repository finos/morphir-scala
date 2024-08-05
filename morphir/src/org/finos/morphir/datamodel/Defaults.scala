package org.finos.morphir.datamodel
import org.finos.morphir.datamodel.Concept.Alias
import org.finos.morphir.datamodel.Concept.Union
import org.finos.morphir.datamodel.Concept.Struct
import org.finos.morphir.datamodel.Concept.Int64
import org.finos.morphir.datamodel.Concept.Decimal
import org.finos.morphir.datamodel.Concept.Int16
import org.finos.morphir.datamodel.Concept.Month
import org.finos.morphir.datamodel.Concept.LocalDate
import org.finos.morphir.datamodel.Concept.LocalTime
import org.finos.morphir.datamodel.Concept.Int32
import org.finos.morphir.datamodel.Concept.Order
import org.finos.morphir.datamodel.Concept.DayOfWeek
import org.finos.morphir.datamodel.Concept.Optional
import org.finos.morphir.datamodel.Concept.Result
import org.finos.morphir.datamodel.Concept.Tuple

sealed trait MDMDefaultError
case class Unimplemented(msg: String)               extends MDMDefaultError
case class NoDefaultNothing()                       extends MDMDefaultError
case class Unfillable(data: Data, concept: Concept) extends MDMDefaultError

trait Defaults {
  def default(concept: Concept): Either[MDMDefaultError, Data] = concept match {
    case concept: Concept.List     => defaultList(concept)
    case concept: Concept.Map      => defaultMap(concept)
    case concept: Concept.Alias    => defaultAlias(concept)
    case concept: Concept.Union    => defaultUnion(concept)
    case concept: Concept.Record   => defaultRecord(concept)
    case concept: Concept.Struct   => defaultStruct(concept)
    case concept: Concept.Enum     => defaultEnum(concept)
    case Concept.Int64             => defaultInt64
    case Concept.Integer           => defaultInteger
    case Concept.Decimal           => defaultDecimal
    case Concept.Int16             => defaultInt16
    case Concept.Nothing           => defaultNothing
    case Concept.Month             => defaultMonth
    case Concept.LocalDate         => defaultLocalDate
    case Concept.Float             => defaultFloat
    case Concept.LocalTime         => defaultLocalTime
    case Concept.Int32             => defaultInt32
    case Concept.Byte              => defaultByte
    case Concept.Unit              => defaultUnit
    case Concept.String            => defaultString
    case Concept.Order             => defaultOrder
    case Concept.DayOfWeek         => defaultDayOfWeek
    case Concept.Char              => defaultChar
    case Concept.Boolean           => defaultBoolean
    case Concept.Any               => defaultAny
    case concept: Concept.Optional => defaultOptional(concept)
    case concept: Concept.Result   => defaultResult(concept)
    case concept: Concept.Set      => defaultSet(concept)
    case concept: Concept.Tuple    => defaultTuple(concept)
  }
  def fillWithDefaults(data: Data, concept: Concept): Either[MDMDefaultError, Data]
  def defaultList(concept: Concept.List): Either[MDMDefaultError, Data] = Right(Data.List.empty(concept.elementType))
  def defaultMap(concept: Concept.Map): Either[MDMDefaultError, Data] =
    Right(Data.Map.empty(concept.keyType, concept.valueType))
  def defaultAlias(concept: Concept.Alias): Either[MDMDefaultError, Data] =
    default(concept.value).map(Data.Aliased(_, concept))
  def defaultUnion(concept: Concept.Union) = default(concept.cases.head).map(Data.Union(_, concept))
  def defaultRecord(concept: Concept.Record): Either[MDMDefaultError, Data] = {
    val empty: Either[MDMDefaultError, List[(Label, Data)]] = Right(List())
    val dataFields = concept.fields.foldLeft(empty) { case (acc, (label, concept)) =>
      acc match {
        case Left(err) => Left(err)
        case Right(l) =>
          default(concept).map(l :+ (label, _))
      }
    }
    dataFields.map(Data.Record(_, concept))
  }
  def defaultStruct(concept: Concept.Struct): Either[MDMDefaultError, Data] = {
    val empty: Either[MDMDefaultError, List[(Label, Data)]] = Right(List())
    val dataFields = concept.fields.foldLeft(empty) { case (acc, (label, concept)) =>
      acc match {
        case Left(err) => Left(err)
        case Right(l) =>
          default(concept).map(l :+ (label, _))
      }
    }
    dataFields.map(Data.Struct(_))
  }
  def defaultEnum(concept: Concept.Enum): Either[MDMDefaultError, Data] = {
    val firstCase                                               = concept.cases.head
    val empty: Either[MDMDefaultError, List[(EnumLabel, Data)]] = Right(List())
    val args = firstCase.fields.foldLeft(empty) { case (acc, (label, concept)) =>
      acc match {
        case Left(err) => Left(err)
        case Right(l) =>
          default(concept).map(l :+ (label, _))
      }
    }
    args.map(Data.Case(_, firstCase.label.toString(), concept))
  }
  def defaultInt64: Either[MDMDefaultError, Data]   = Right(Data.Int64(0))
  def defaultInteger: Either[MDMDefaultError, Data] = Right(Data.Integer(BigInt(0)))
  def defaultDecimal: Either[MDMDefaultError, Data] = Right(Data.Decimal(BigDecimal(0)))
  def defaultInt16: Either[MDMDefaultError, Data]   = Right(Data.Int16(0))
  def defaultNothing: Either[MDMDefaultError, Data] = Left(NoDefaultNothing())
  def defaultMonth: Either[MDMDefaultError, Data]   = Right(Data.Month(java.time.Month.JANUARY))
  def defaultLocalDate: Either[MDMDefaultError, Data] =
    Right(Data.LocalDate(java.time.LocalDate.EPOCH))
  def defaultFloat: Either[MDMDefaultError, Data] = Right(Data.Float(0))
  def defaultLocalTime: Either[MDMDefaultError, Data] =
    Right(Data.LocalTime(java.time.LocalTime.MIDNIGHT))
  def defaultInt32: Either[MDMDefaultError, Data]  = Right(Data.Int32(0))
  def defaultByte: Either[MDMDefaultError, Data]   = Right(Data.Byte(0))
  def defaultUnit: Either[MDMDefaultError, Data]   = Right(Data.Unit)
  def defaultString: Either[MDMDefaultError, Data] = Right(Data.String(""))
  def defaultOrder: Either[MDMDefaultError, Data]  = Right(Data.Order.apply(0))
  def defaultDayOfWeek: Either[MDMDefaultError, Data] =
    Right(Data.DayOfWeek(java.time.DayOfWeek.of(0)))
  def defaultChar: Either[MDMDefaultError, Data]    = Right(Data.Char('0'))
  def defaultBoolean: Either[MDMDefaultError, Data] = Right(Data.True)
  def defaultOptional(concept: Concept.Optional): Either[MDMDefaultError, Data] =
    Right(Data.Optional.None(concept.elementType))
  def defaultAny: Either[MDMDefaultError, Data] = Right(Data.Unit)
  def defaultResult(concept: Concept.Result): Either[MDMDefaultError, Data] =
    default(concept.okType).map(Data.Result.Ok(_, concept))
  def defaultSet(concept: Concept.Set): Either[MDMDefaultError, Data] = Right(Data.Set.empty(concept.elementType))
  def defaultTuple(concept: Concept.Tuple): Either[MDMDefaultError, Data] = {
    val empty: Either[MDMDefaultError, List[Data]] = Right(List())
    val elems = concept.values.foldLeft(empty) { case (acc, concept) =>
      acc match {
        case Left(err) => Left(err)
        case Right(l) =>
          default(concept).map(l :+ _)
      }
    }
    elems.map(Data.Tuple(_))
  }
}

trait DefaultFiller extends Defaults {
  def fillWithDefaults(data: Data, concept: Concept): Either[MDMDefaultError, Data]
}

case class DefaultOptions()
case class FillOptions()
case class MDMDefaults(defaultOptions: DefaultOptions, fillOptions: FillOptions) extends DefaultFiller {

  def fillWithDefaults(data: Data, concept: Concept): Either[MDMDefaultError, Data] =
    (data, concept) match {
      case (otherData, otherConcept) if otherData.shape == otherConcept =>
        Right(otherData) // Hopefully == is good enough
      case (recordData: Data.Record, recordConcept: Concept.Record) =>
        val empty: Either[MDMDefaultError, List[(Label, Data)]] = Right(List())
        val dataFields = recordConcept.fields.foldLeft(empty) { case (acc, (label, fieldConcept)) =>
          acc match {
            case Left(err) => Left(err)
            case Right(l) =>
              val newField = recordData.values.find(_._1 == label) match {
                case None                 => default(fieldConcept).map((label, _))
                case Some((_, fieldData)) => fillWithDefaults(fieldData, fieldConcept).map((label, _))
              }
              newField.map(l :+ _)
          }
        }
        dataFields.map(Data.Record(_, recordConcept))
      case (concept: Concept.List)   => defaultList(concept)
      case concept: Concept.Map      => defaultMap(concept)
      case concept: Concept.Alias    => defaultAlias(concept)
      case concept: Concept.Union    => defaultUnion(concept)
      case concept: Concept.Record   => defaultRecord(concept)
      case concept: Concept.Struct   => defaultStruct(concept)
      case concept: Concept.Enum     => defaultEnum(concept)
      case Concept.Int64             => defaultInt64
      case Concept.Integer           => defaultInteger
      case Concept.Decimal           => defaultDecimal
      case Concept.Int16             => defaultInt16
      case Concept.Nothing           => defaultNothing
      case Concept.Month             => defaultMonth
      case Concept.LocalDate         => defaultLocalDate
      case Concept.Float             => defaultFloat
      case Concept.LocalTime         => defaultLocalTime
      case Concept.Int32             => defaultInt32
      case Concept.Byte              => defaultByte
      case Concept.Unit              => defaultUnit
      case Concept.String            => defaultString
      case Concept.Order             => defaultOrder
      case Concept.DayOfWeek         => defaultDayOfWeek
      case Concept.Char              => defaultChar
      case Concept.Boolean           => defaultBoolean
      case Concept.Any               => defaultAny
      case concept: Concept.Optional => defaultOptional(concept)
      case concept: Concept.Result   => defaultResult(concept)
      case concept: Concept.Set      => defaultSet(concept)
      case concept: Concept.Tuple    => defaultTuple(concept)

      case (otherData, otherConcept) => Left(Unfillable(otherData, otherConcept))
    }

}
