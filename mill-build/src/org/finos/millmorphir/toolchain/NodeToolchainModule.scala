package org.finos.millmorphir.toolchain

import java.net.URI

import mill.*

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
    VerifiedArchive.downloadAndExtract(url, distribution.sha256, distribution.format, Task.dest)
    PathRef(Task.dest)
  }

  def nodeExecutable: T[PathRef] = Task {
    PathRef(nodeHome().path / nodeDistribution().nodeRelativePath)
  }

  def npmCli: T[PathRef] = Task {
    PathRef(nodeHome().path / nodeDistribution().npmCliRelativePath)
  }
}
