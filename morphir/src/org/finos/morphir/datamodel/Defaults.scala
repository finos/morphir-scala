package org.finos.morphir.datamodel
import zio.*

type DefaultTask = Task[Data]

sealed trait MDMDefaultError                        extends Throwable
case class Unimplemented(msg: String)               extends MDMDefaultError
case class NoDefaultNothing()                       extends MDMDefaultError
case class Unfillable(data: Data, concept: Concept) extends MDMDefaultError

trait Defaults {
  def default(concept: Concept): DefaultTask = concept match {
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
  def fillWithDefaults(data: Data, concept: Concept): DefaultTask
  def defaultList(concept: Concept.List): DefaultTask = ZIO.succeed(Data.List.empty(concept.elementType))
  def defaultMap(concept: Concept.Map): DefaultTask =
    ZIO.succeed(Data.Map.empty(concept.keyType, concept.valueType))
  def defaultAlias(concept: Concept.Alias): DefaultTask =
    default(concept.value).map(Data.Aliased(_, concept))
  def defaultUnion(concept: Concept.Union) = default(concept.cases.head).map(Data.Union(_, concept))
  def defaultRecord(concept: Concept.Record): DefaultTask = for {
    dataFields <- ZIO.collectAll(concept.fields.map { (label, concept) => default(concept).map((label, _)) })
    res = Data.Record(dataFields, concept)
  } yield res
  def defaultStruct(concept: Concept.Struct): DefaultTask = for {
    dataFields <- ZIO.collectAll(concept.fields.map { (label, concept) => default(concept).map((label, _)) })
    res = Data.Struct(dataFields)
  } yield res

  def defaultEnum(concept: Concept.Enum): DefaultTask = {
    val firstCase = concept.cases.head
    for {
      args <- ZIO.collectAll(firstCase.fields.map { (label, concept) => default(concept).map((label, _)) })
      res = Data.Case(args, firstCase.label.toString(), concept)
    } yield res
  }
  def defaultInt64: DefaultTask   = ZIO.succeed(Data.Int64(0))
  def defaultInteger: DefaultTask = ZIO.succeed(Data.Integer(BigInt(0)))
  def defaultDecimal: DefaultTask = ZIO.succeed(Data.Decimal(BigDecimal(0)))
  def defaultInt16: DefaultTask   = ZIO.succeed(Data.Int16(0))
  def defaultNothing: DefaultTask = ZIO.fail(NoDefaultNothing())
  def defaultMonth: DefaultTask   = ZIO.succeed(Data.Month(java.time.Month.JANUARY))
  def defaultLocalDate: DefaultTask =
    ZIO.succeed(Data.LocalDate(java.time.LocalDate.EPOCH))
  def defaultFloat: DefaultTask = ZIO.succeed(Data.Float(0))
  def defaultLocalTime: DefaultTask =
    ZIO.succeed(Data.LocalTime(java.time.LocalTime.MIDNIGHT))
  def defaultInt32: DefaultTask  = ZIO.succeed(Data.Int32(0))
  def defaultByte: DefaultTask   = ZIO.succeed(Data.Byte(0))
  def defaultUnit: DefaultTask   = ZIO.succeed(Data.Unit)
  def defaultString: DefaultTask = ZIO.succeed(Data.String(""))
  def defaultOrder: DefaultTask  = ZIO.succeed(Data.Order.apply(0))
  def defaultDayOfWeek: DefaultTask =
    ZIO.succeed(Data.DayOfWeek(java.time.DayOfWeek.of(0)))
  def defaultChar: DefaultTask    = ZIO.succeed(Data.Char('0'))
  def defaultBoolean: DefaultTask = ZIO.succeed(Data.True)
  def defaultOptional(concept: Concept.Optional): DefaultTask =
    ZIO.succeed(Data.Optional.None(concept.elementType))
  def defaultAny: DefaultTask = ZIO.succeed(Data.Unit)
  def defaultResult(concept: Concept.Result): DefaultTask =
    default(concept.okType).map(Data.Result.Ok(_, concept))
  def defaultSet(concept: Concept.Set): DefaultTask = ZIO.succeed(Data.Set.empty(concept.elementType))
  def defaultTuple(concept: Concept.Tuple): DefaultTask = for {
    elems <- ZIO.collectAll(concept.values.map(default))
    res = Data.Tuple(elems)
  } yield res
}

trait DefaultFiller extends Defaults {
  def fillWithDefaults(data: Data, concept: Concept): DefaultTask
}

object MDMDefaults extends DefaultFiller {

  def fillWithDefaults(data: Data, concept: Concept): DefaultTask =
    (data, concept) match {
      case (otherData, otherConcept) if otherData.shape == otherConcept =>
        ZIO.succeed(otherData) // Hopefully == is good enough
      case (recordData: Data.Record, recordConcept: Concept.Record) => for {
          dataFields <- ZIO.collectAll(recordConcept.fields.map { case (label, fieldConcept) =>
            recordData.values.find(_._1 == label) match {
              case None                 => default(fieldConcept).map((label, _))
              case Some((_, fieldData)) => fillWithDefaults(fieldData, fieldConcept).map((label, _))
            }
          })
          res = Data.Record(dataFields, recordConcept)
        } yield res
      // TODO: Other cases
      case (otherData, otherConcept) => ZIO.fail(Unfillable(otherData, otherConcept))
    }

}
