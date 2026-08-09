package org.finos.morphir.mill.toolchain

/** Resource limits applied while acquiring verified content. */
final case class AcquisitionLimits(maxAcquiredBytes: StorageSize = storageSize"2 GiB") {
  require(maxAcquiredBytes.toBytes > 0, "maxAcquiredBytes must be positive")
}
