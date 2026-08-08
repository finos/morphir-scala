package org.finos.morphir.mill.toolchain

import scala.compiletime.testing.typeCheckErrors
import utest.*

object StorageSizeTests extends TestSuite {
  private def parsed(value: String): StorageSize =
    StorageSize.parse(value).fold(throw _, identity)

  val tests = Tests {
    test("runtime parser accepts exact byte, SI, and IEC quantities") {
      val expected = Seq(
        "0"       -> 0L,
        "512 B"   -> 512L,
        "1 KB"    -> 1000L,
        "2 MB"    -> 2000000L,
        "3 GB"    -> 3000000000L,
        "1 KiB"   -> 1024L,
        "64 MiB"  -> 67108864L,
        "2 GiB"   -> 2147483648L,
        "1 TiB"   -> 1099511627776L,
        "1.5 KiB" -> 1536L
      )

      expected.foreach { case (input, bytes) =>
        assert(parsed(input).toBytes == bytes)
      }
    }

    test("runtime parser rejects invalid or inexact quantities") {
      val rejected = Seq(
        "-1 B",
        "-1",
        "0.1 B",
        "1.0001 KB",
        "1 XB",
        "9223372036854775808",
        "8388608 TiB",
        ""
      )

      rejected.foreach { input =>
        val error = StorageSize.parse(input).swap.toOption.get
        assert(error.isInstanceOf[IllegalArgumentException])
        assert(error.input == input)
      }
    }

    test("human rendering is lossless") {
      val expected = Seq(
        "0"                    -> "0 B",
        "1000"                 -> "1 KB",
        "1024"                 -> "1 KiB",
        "1000000"              -> "1 MB",
        "1048576"              -> "1 MiB",
        "1073741824"           -> "1 GiB",
        "1536"                 -> "1536 B",
        Long.MaxValue.toString -> s"${Long.MaxValue} B"
      )

      expected.foreach { case (input, rendered) =>
        val size = parsed(input)
        assert(size.show == rendered)
        assert(StorageSize.parse(size.show) == Right(size))
      }
    }

    test("literal interpolator validates and constructs at compile time") {
      val limit = storageSize"64 MiB"
      assert(limit.toBytes == 64L * 1024 * 1024)

      val negative = typeCheckErrors(
        """import org.finos.morphir.mill.toolchain.*; storageSize"-1 B""""
      )
      val inexact = typeCheckErrors(
        """import org.finos.morphir.mill.toolchain.*; storageSize"0.1 B""""
      )
      val overflow = typeCheckErrors(
        """import org.finos.morphir.mill.toolchain.*; storageSize"8388608 TiB""""
      )
      val interpolated = typeCheckErrors(
        """import org.finos.morphir.mill.toolchain.*; val value = 64; storageSize"$value MiB""""
      )

      assert(negative.nonEmpty)
      assert(inexact.nonEmpty)
      assert(overflow.nonEmpty)
      assert(interpolated.nonEmpty)
      assert(interpolated.head.message.contains("does not accept interpolation"))
    }

    test("public toolchain byte limits use StorageSize") {
      val acquired: StorageSize    = AcquisitionLimits().maxAcquiredBytes
      val archive                  = ArchiveLimits()
      val entry: StorageSize       = archive.maxEntryUncompressedBytes
      val total: StorageSize       = archive.maxTotalUncompressedBytes
      val link: StorageSize        = archive.maxSymlinkTargetBytes
      val compressed: StorageSize  = archive.maxCompressedArchiveBytes
      val central: StorageSize     = archive.maxCentralDirectoryBytes
      val metadata: StorageSize    = archive.maxMetadataEntryBytes
      val allMetadata: StorageSize = archive.maxArchiveMetadataBytes
      val gzipHeader: StorageSize  = archive.maxGzipHeaderBytes

      assert(acquired.toBytes > 0L)
      assert(entry.toBytes > 0L)
      assert(total.toBytes > 0L)
      assert(link.toBytes > 0L)
      assert(compressed.toBytes > 0L)
      assert(central.toBytes > 0L)
      assert(metadata.toBytes > 0L)
      assert(allMetadata.toBytes > 0L)
      assert(gzipHeader.toBytes > 0L)
    }
  }
}
