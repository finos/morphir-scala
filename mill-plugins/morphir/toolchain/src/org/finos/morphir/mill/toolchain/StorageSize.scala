package org.finos.morphir.mill.toolchain

import java.util.Locale
import upickle.default.ReadWriter

/** A non-negative quantity of storage represented exactly in bytes. */
opaque type StorageSize = Long

object StorageSize {
  inline given CanEqual[StorageSize, StorageSize] = CanEqual.derived

  given ReadWriter[StorageSize] =
    upickle.default.readwriter[Long].bimap[StorageSize](_.toBytes, value => fromBytes(value).fold(throw _, identity))

  final case class Error(input: String, reason: String)
      extends IllegalArgumentException(s"Invalid storage size '$input': $reason")

  private final case class UnitOfMeasure(symbol: String, bytes: Long)

  private val Bytes = UnitOfMeasure("B", 1L)
  private val Units = Seq(
    Bytes,
    UnitOfMeasure("KB", 1000L),
    UnitOfMeasure("MB", 1000000L),
    UnitOfMeasure("GB", 1000000000L),
    UnitOfMeasure("KiB", 1L << 10),
    UnitOfMeasure("MiB", 1L << 20),
    UnitOfMeasure("GiB", 1L << 30),
    UnitOfMeasure("TiB", 1L << 40)
  )
  private val RenderUnits = Units.sortBy(_.bytes)(using Ordering.Long.reverse)
  private val UnitByName  = Units.iterator.map(unit => unit.symbol.toLowerCase(Locale.ROOT) -> unit).toMap
  private val Input       = "([0-9]+(?:\\.[0-9]+)?)\\s*([A-Za-z]*)".r

  val Zero: StorageSize = 0L

  def fromBytes(value: Long): Either[Error, StorageSize] =
    if (value < 0L) Left(Error(value.toString, "size cannot be negative"))
    else Right(value)

  def parse(input: String): Either[Error, StorageSize] =
    input.trim match {
      case Input(number, unitName) =>
        val unit =
          if (unitName.isEmpty) Some(Bytes)
          else UnitByName.get(unitName.toLowerCase(Locale.ROOT))
        unit.toRight(Error(input, s"unknown unit '$unitName'")).flatMap { selected =>
          val bytes = BigDecimal(number) * BigDecimal(selected.bytes)
          bytes.toBigIntExact match {
            case None => Left(Error(input, "quantity does not resolve to a whole number of bytes"))
            case Some(value) if value > BigInt(Long.MaxValue) =>
              Left(Error(input, "quantity exceeds Long.MaxValue bytes"))
            case Some(value) => Right(value.toLong)
          }
        }
      case _ => Left(Error(input, "expected a non-negative number followed by B, KB, MB, GB, KiB, MiB, GiB, or TiB"))
    }

  extension (self: StorageSize) {
    def toBytes: Long = self

    def show: String = {
      val unit =
        if (self == 0L) Bytes
        else RenderUnits.find(candidate => self % candidate.bytes == 0L).getOrElse(Bytes)
      s"${self / unit.bytes} ${unit.symbol}"
    }
  }
}

extension (inline context: StringContext)
  inline def storageSize(inline arguments: Any*): StorageSize =
    ${ StorageSizeInterpolator.expand('context, 'arguments) }
