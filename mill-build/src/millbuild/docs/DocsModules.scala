package millbuild.docs

import mill.*
import mill.scalalib.*
import os.Path

trait MDocModule extends ScalaModule {

  def scalaMdocVersion: T[String] = Task { "2.5.2" }

  def scalaMdocDep: T[Dep] = Task { mvn"org.scalameta::mdoc:${scalaMdocVersion()}" }

  def watchedMDocsDestination: T[Option[Path]] = Task { None }

  override def mvnDeps = Task {
    super.mvnDeps() ++ Seq(scalaMdocDep())
  }

  // where do the mdoc sources live ?
  def mdocSources = Task.Sources(moduleDir)

  def mdoc: T[PathRef] = Task {

    val cp = runClasspath().map(_.path)

    val dir = Task.dest.toIO.getAbsolutePath
    val dirParams =
      mdocSources().map(pr => Seq(s"--in", pr.path.toIO.getAbsolutePath, "--out", dir)).iterator.flatten.toSeq

    os.proc("java", "-cp", cp.mkString(java.io.File.pathSeparator), "mdoc.Main", dirParams).call(cwd = Task.dest)

    PathRef(Task.dest)
  }

  def mdocWatch() = Task.Command {

    watchedMDocsDestination() match {
      case None =>
        throw new Exception("watchedMDocsDestination is not set, so we dant know where to put compiled md files")
      case Some(p) =>
        val cp = runClasspath().map(_.path)
        val dirParams = mdocSources()
          .map(pr => Seq(s"--in", pr.path.toIO.getAbsolutePath, "--out", p.toIO.getAbsolutePath))
          .iterator
          .flatten
          .toSeq
        os.proc("java", "-cp", cp.mkString(java.io.File.pathSeparator), "mdoc.Main", dirParams ++ Seq("--watch")).call(cwd = Task.dest)
    }

  }
}

trait Docusaurus2Module extends Module {

  def docusaurusSources: T[Seq[PathRef]]
  def compiledMdocs: T[Seq[PathRef]]

  def yarnInstall: T[PathRef] = Task {
    val baseDir = Task.dest

    docusaurusSources().foreach { pr =>
      os.list(pr.path)
        .foreach(p =>
          os.copy.into(
            p,
            baseDir,
            followLinks = true,
            replaceExisting = true,
            copyAttributes = true,
            createFolders = true,
            mergeFolders = false
          )
        )
    }

    val result = os.proc("yarn", "install", "--check-cache").call(cwd = Task.dest)
    Task.log.info(result.out.text())
    PathRef(Task.dest)
  }

  def installedDocusaurusSources: T[PathRef] = Task { yarnInstall() }

  def docusaurusBuild: T[PathRef] = Task {
    val workDir                = Task.dest
    val docusaurusInstallation = installedDocusaurusSources()
    val yarnSetup              = docusaurusInstallation.path

    os.copy(
      yarnSetup,
      workDir,
      followLinks = true,
      replaceExisting = true,
      copyAttributes = true,
      createFolders = true,
      mergeFolders = false
    )

    val docsDir = workDir / "docs"
    os.makeDir.all(docsDir)
    os.list(docsDir).foreach(os.remove.all)

    Seq(docusaurusInstallation).foreach { pr =>
      val bd = pr.path
      os.walk(pr.path / "docs").foreach { p =>
        val relPath = p.relativeTo(bd / "docs")
        Task.log.info(relPath.toString())
        if (p.toIO.isFile) {
          val target = docsDir / relPath
          os.makeDir.all(target)
          os.copy.over(p, docsDir / relPath)
        }
      }
    }

    compiledMdocs().foreach { pr =>
      os.list(pr.path)
        .foreach(p =>
          os.copy.into(
            p,
            docsDir,
            followLinks = true,
            replaceExisting = true,
            copyAttributes = true,
            createFolders = true,
            mergeFolders = true
          )
        )
    }

    val buildResult = os.proc("yarn", "build").call(cwd = workDir)
    Task.log.info(buildResult.out.text())

    PathRef(workDir)
  }

  def docusaurusServe() = Task.Command {
    val workDir = docusaurusBuild().path
    os.proc("yarn", "start").call(cwd = workDir, env = Task.env)
  }
}
