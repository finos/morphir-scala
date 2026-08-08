package org.finos.morphir.mill.toolchain

/** Resource limits applied while extracting a verified archive. Defaults accommodate Node distributions. */
final case class ArchiveLimits(
    maxEntries: Long = 100000L,
    maxEntryUncompressedBytes: Long = 2L * 1024 * 1024 * 1024,
    maxTotalUncompressedBytes: Long = 8L * 1024 * 1024 * 1024,
    maxCompressionRatio: Double = 200.0,
    maxSymlinkTargetBytes: Int = 4096,
    maxCompressedArchiveBytes: Long = 1024L * 1024 * 1024
) {
  require(maxEntries > 0, "maxEntries must be positive")
  require(maxEntryUncompressedBytes > 0, "maxEntryUncompressedBytes must be positive")
  require(maxTotalUncompressedBytes > 0, "maxTotalUncompressedBytes must be positive")
  require(maxCompressionRatio.isFinite && maxCompressionRatio >= 1.0, "maxCompressionRatio must be finite and >= 1")
  require(maxSymlinkTargetBytes > 0, "maxSymlinkTargetBytes must be positive")
  require(maxCompressedArchiveBytes > 0, "maxCompressedArchiveBytes must be positive")
}
