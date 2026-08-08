package org.finos.morphir.mill.javascript.node

import java.io.BufferedInputStream
import java.net.URI
import java.nio.file.Paths

import mill.*
import org.finos.morphir.mill.javascript.{JavaScriptCommand, JavaScriptRuntimeModule}
import org.finos.morphir.mill.toolchain.{AcquisitionCache, AcquisitionSettings, VerifiedArchive}

trait NodeRuntimeModule extends JavaScriptRuntimeModule {
  def runtimeVersion: T[String] = Task { NodeDistribution.Version }

  def nodeMachineCacheRoot: T[String] = Task.Input {
    Task.env.getOrElse("MORPHIR_NODE_CACHE", "")
  }

  def nodeUseMachineCache: T[Boolean] = Task.Input {
    !NodeRuntimeModule.enabled(Task.env.get("MORPHIR_NODE_DISABLE_MACHINE_CACHE"))
  }

  def nodeOffline: T[Boolean] = Task.Input {
    Task.offline || NodeRuntimeModule.enabled(Task.env.get("MORPHIR_NODE_OFFLINE"))
  }

  def nodeDistribution: T[NodeDistribution] = Task {
    NodeDistribution
      .resolve(runtimeVersion(), System.getProperty("os.name"), System.getProperty("os.arch"))
      .fold(message => throw new IllegalArgumentException(message), identity)
  }

  def runtimeHome: T[PathRef] = Task {
    val cacheOverride = nodeMachineCacheRoot()
    val cacheRoot     = Option(cacheOverride).filter(_.nonEmpty).map { value =>
      val path = Paths.get(value)
      if (!path.isAbsolute)
        throw new IllegalArgumentException(s"MORPHIR_NODE_CACHE must be an absolute path: '$value'")
      os.Path(path)
    }
    NodeRuntimeModule.provision(
      nodeDistribution(),
      AcquisitionSettings(cacheRoot, nodeUseMachineCache(), nodeOffline()),
      Task.dest
    )
  }

  def runtimeExecutable: T[PathRef] = Task {
    PathRef(runtimeHome().path / nodeDistribution().nodeRelativePath)
  }

  def runtimeCommand(arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    NodeProcess.runtime(runtimeExecutable(), arguments)
  }

  final def nodeVersion: T[String]     = runtimeVersion
  final def nodeHome: T[PathRef]       = runtimeHome
  final def nodeExecutable: T[PathRef] = runtimeExecutable

  def npmCli: T[PathRef] = Task {
    PathRef(runtimeHome().path / nodeDistribution().npmCliRelativePath)
  }
}

object NodeRuntimeModule {
  private[javascript] def enabled(value: Option[String]): Boolean =
    value.exists(candidate => Set("1", "true", "yes", "on").contains(candidate.toLowerCase(java.util.Locale.ROOT)))

  private[javascript] def provision(
      distribution: NodeDistribution,
      settings: AcquisitionSettings,
      taskRoot: os.Path
  ): PathRef = {
    val url     = URI.create(s"https://nodejs.org/dist/v${distribution.version}/${distribution.archiveName}").toURL
    val content = AcquisitionCache(settings, taskRoot / "acquisition").acquire(distribution.sha256, url.toString) {
      val connection = url.openConnection()
      connection.setConnectTimeout(30000)
      connection.setReadTimeout(60000)
      new BufferedInputStream(connection.getInputStream)
    }
    val destination = taskRoot / "node"
    VerifiedArchive.extract(content, distribution.format, destination)
    PathRef(destination)
  }
}
