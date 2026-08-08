package org.finos.morphir.mill.toolchain

/** Resource limits applied while acquiring verified content. */
final case class AcquisitionLimits(maxAcquiredBytes: Long = 2L * 1024 * 1024 * 1024) {
  require(maxAcquiredBytes > 0, "maxAcquiredBytes must be positive")
}
