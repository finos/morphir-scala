package org.finos.morphir.launcher

import coursier.cache.FileCache
import coursier.cache.loggers.RefreshLogger
import coursier.util.Task
import coursier.{Dependency, Fetch}

/**
 * Wrapper for Coursier Fetch to facilitate logging progress to stderr and testing.
 */
trait Coursier {
  def fetch(deps: Dependency*): Unit
}

// Avoid using, for instance, ZIO, for module implementation to keep dependencies minimal.
case object CoursierLive extends Coursier {
  private def loggingFetch = {
    val logger          = RefreshLogger.create()
    val cacheWithLogger = FileCache[Task]().withLogger(logger)
    Fetch().withCache(cacheWithLogger)
  }

  def fetch(deps: Dependency*): Unit = loggingFetch.withDependencies(deps).run()
}
