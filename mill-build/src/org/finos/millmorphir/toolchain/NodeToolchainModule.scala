package org.finos.millmorphir.toolchain

import java.io.BufferedInputStream
import java.net.URI

import mill.*
import org.finos.morphir.mill.toolchain.{AcquisitionCache, AcquisitionSettings, VerifiedArchive}

trait NodeToolchainModule extends Module {
  def nodeVersion: T[String] = Task { NodeDistribution.Version }

  def nodeDistribution: T[NodeDistribution] = Task {
    NodeDistribution
      .resolve(System.getProperty("os.name"), System.getProperty("os.arch"))
      .fold(message => throw new IllegalArgumentException(message), identity)
  }

  def nodeHome: T[PathRef] = Task {
    val distribution = nodeDistribution()
    val url          = URI.create(s"https://nodejs.org/dist/v${distribution.version}/${distribution.archiveName}").toURL
    val content      = AcquisitionCache(AcquisitionSettings(), Task.dest).acquire(distribution.sha256, url.toString) {
      val connection = url.openConnection()
      connection.setConnectTimeout(30000)
      connection.setReadTimeout(60000)
      new BufferedInputStream(connection.getInputStream)
    }
    VerifiedArchive.extract(content, distribution.format, Task.dest)
    PathRef(Task.dest)
  }

  def nodeExecutable: T[PathRef] = Task {
    PathRef(nodeHome().path / nodeDistribution().nodeRelativePath)
  }

  def npmCli: T[PathRef] = Task {
    PathRef(nodeHome().path / nodeDistribution().npmCliRelativePath)
  }
}
