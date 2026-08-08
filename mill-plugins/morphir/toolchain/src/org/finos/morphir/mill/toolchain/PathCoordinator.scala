package org.finos.morphir.mill.toolchain

import java.io.IOException
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.StandardOpenOption
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock
import scala.util.Using

private[toolchain] object PathCoordinator {
  // Entries intentionally persist: unsafe eviction could create two local locks for one path concurrently.
  private val LocalLocks             = new ConcurrentHashMap[String, ReentrantLock]()
  private val InitialLockDelayMillis = 10L
  private val MaximumLockDelayMillis = 250L

  def withLock[A](lockPath: os.Path)(operation: => A): A = {
    os.makeDir.all(lockPath / os.up)
    val lockKey = lockPath.toNIO.getParent.toRealPath().resolve(lockPath.last).toString
    val local   = LocalLocks.computeIfAbsent(lockKey, _ => new ReentrantLock())
    try local.lockInterruptibly()
    catch {
      case error: InterruptedException => throw interrupted(lockPath, error)
    }
    try
      Using.resource(
        FileChannel.open(lockPath.toNIO, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
      ) { channel =>
        Using.resource(acquireFileLock(channel, lockPath))(_ => operation)
      }
    finally local.unlock()
  }

  private def acquireFileLock(channel: FileChannel, lockPath: os.Path): FileLock = {
    var delay    = InitialLockDelayMillis
    var acquired = Option.empty[FileLock]
    while (acquired.isEmpty) {
      acquired = Option(
        try channel.tryLock()
        catch { case _: OverlappingFileLockException => null }
      )
      if (acquired.isEmpty) {
        try Thread.sleep(delay)
        catch {
          case error: InterruptedException => throw interrupted(lockPath, error)
        }
        delay = math.min(delay * 2, MaximumLockDelayMillis)
      }
    }
    acquired.get
  }

  private def interrupted(lockPath: os.Path, cause: InterruptedException): IOException = {
    Thread.currentThread().interrupt()
    new IOException(s"Interrupted while waiting for coordination lock $lockPath", cause)
  }
}
