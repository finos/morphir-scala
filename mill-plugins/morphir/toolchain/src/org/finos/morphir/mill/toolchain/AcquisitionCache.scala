package org.finos.morphir.mill.toolchain

import java.io.InputStream
import java.nio.channels.FileChannel
import java.nio.file.{Files, StandardCopyOption, StandardOpenOption}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.util.Using

final class AcquisitionCache(settings: AcquisitionSettings, taskRoot: os.Path) {
  def acquire(expectedSha256: String, source: String)(openStream: => InputStream): VerifiedContent = {
    val digest = normalizedDigest(expectedSha256)
    val root   =
      if (settings.useMachineCache) settings.cacheRoot.getOrElse(AcquisitionSettings.defaultCacheRoot)
      else taskRoot / ".morphir-acquisitions"
    val entry = root / "sha256" / digest
    os.makeDir.all(entry / os.up)
    coordinated(entry) {
      acquireLocked(entry, digest, source)(openStream)
    }
  }

  private def acquireLocked(entry: os.Path, digest: String, source: String)(
      openStream: => InputStream
  ): VerifiedContent = {
    if (os.isFile(entry) && scala.util.Try(VerifiedArchive.verifySha256(entry, digest)).isSuccess)
      return VerifiedContent(entry, digest)
    if (settings.offline)
      throw new IllegalStateException(
        s"Offline acquisition cannot use $source: no verified cached content for SHA-256 $digest"
      )
    val temporary = entry / os.up / s".${entry.last}.${UUID.randomUUID()}.tmp"
    try {
      Using
        .Manager { use =>
          val input  = use(openStream)
          val output = use(
            Files.newOutputStream(temporary.toNIO, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
          )
          input.transferTo(output)
        }
        .get
      VerifiedArchive.verifySha256(temporary, digest)
      Files.move(
        temporary.toNIO,
        entry.toNIO,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      VerifiedContent(entry, digest)
    } catch {
      case error: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Verified acquisition failed for $source: ${error.getMessage}", error)
    } finally os.remove(temporary)
  }

  private def coordinated[A](entry: os.Path)(operation: => A): A = {
    val lockPath = entry / os.up / s".${entry.last}.lock"
    val lockKey  = lockPath.toNIO.getParent.toRealPath().resolve(lockPath.last).toString
    val monitor  = AcquisitionCache.LocalLocks.computeIfAbsent(lockKey, _ => new Object)
    monitor.synchronized {
      Using.resource(
        FileChannel.open(lockPath.toNIO, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
      ) { channel =>
        Using.resource(channel.lock())(_ => operation)
      }
    }
  }

  private def normalizedDigest(value: String): String = {
    val normalized = value.toLowerCase(java.util.Locale.ROOT)
    if (!normalized.matches("[0-9a-f]{64}"))
      throw new IllegalArgumentException(s"Invalid SHA-256 digest for acquisition: '$value'")
    normalized
  }
}

object AcquisitionCache {
  private val LocalLocks = new ConcurrentHashMap[String, Object]()

  def apply(settings: AcquisitionSettings, taskRoot: os.Path): AcquisitionCache =
    new AcquisitionCache(settings, taskRoot)
}
