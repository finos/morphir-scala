package org.finos.morphir.mill.javascript.npm

import java.nio.file.{Files, LinkOption}
import scala.jdk.CollectionConverters.*
import scala.util.Using

import mill.*
import org.finos.morphir.mill.javascript.*
import org.finos.morphir.mill.javascript.node.{NodeProcess, NodeRuntimeModule}

trait NpmPackageManagerModule extends JavaScriptPackageManagerModule {
  def runtime: NodeRuntimeModule

  def packageJson: T[PathRef]     = Task.Source(moduleDir / "package.json")
  def packageLockJson: T[PathRef] = Task.Source(moduleDir / "package-lock.json")

  def projectFiles: T[Seq[PathRef]] = Task {
    Seq(packageJson())
  }

  def lockFiles: T[Seq[PathRef]] = Task {
    Seq(packageLockJson())
  }

  def npmEnvironmentInputs: T[Seq[(String, String)]] = Task.Input {
    NpmProcess.retainedEnvironment(Task.env).toSeq.sorted
  }

  def install: T[JavaScriptInstall] = Task {
    val _           = runtime.runtimeVersion()
    val prepared    = NpmProcess.prepareInstall(Task.dest / "install", projectFiles(), lockFiles())
    val command     = NpmProcess.ci(runtime.runtimeExecutable(), runtime.npmCli(), Task.dest / "npm-cache")
    val environment = NpmProcess.environment(Task.dest / "process-state", npmEnvironmentInputs().toMap)
    NpmProcess.initialize(environment)
    os.proc(command.executable.path.toString +: command.arguments)
      .call(cwd = prepared.root.path, env = environment, propagateEnv = false)
    prepared
  }

  def packageManagerCommand(arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    NodeProcess.npm(runtime.runtimeExecutable(), runtime.npmCli(), arguments)
  }

  def packageBinaryCommand(binary: PackageBinary, arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    NpmProcess.binary(runtime.runtimeExecutable(), install(), binary, arguments)
  }
}

private[javascript] object NpmProcess {
  private val RetainedVariables = Set(
    "HTTP_PROXY",
    "HTTPS_PROXY",
    "NO_PROXY",
    "ALL_PROXY",
    "http_proxy",
    "https_proxy",
    "no_proxy",
    "all_proxy",
    "SSL_CERT_FILE",
    "SSL_CERT_DIR",
    "NODE_EXTRA_CA_CERTS",
    "SYSTEMROOT",
    "SystemRoot",
    "WINDIR",
    "COMSPEC",
    "PATHEXT"
  )

  def retainedEnvironment(ambient: Map[String, String]): Map[String, String] =
    ambient.view.filterKeys(RetainedVariables).toMap

  def environment(taskRoot: os.Path, retained: Map[String, String]): Map[String, String] = {
    val home  = taskRoot / "home"
    val cache = taskRoot / "cache"
    val temp  = taskRoot / "tmp"
    retainedEnvironment(retained) ++ Map(
      "HOME"             -> home.toString,
      "USERPROFILE"      -> home.toString,
      "XDG_CACHE_HOME"   -> (cache / "xdg").toString,
      "npm_config_cache" -> (cache / "npm").toString,
      "TMPDIR"           -> temp.toString,
      "TMP"              -> temp.toString,
      "TEMP"             -> temp.toString
    )
  }

  def initialize(environment: Map[String, String]): Unit =
    Seq("HOME", "XDG_CACHE_HOME", "npm_config_cache", "TMPDIR")
      .foreach(name => os.makeDir.all(os.Path(environment(name), os.pwd)))

  def prepareInstall(root: os.Path, projectFiles: Seq[PathRef], lockFiles: Seq[PathRef]): JavaScriptInstall = {
    if (lockFiles.isEmpty)
      throw new IllegalArgumentException("npm install requires a committed npm lock file")
    val allFiles = projectFiles ++ lockFiles
    allFiles.foreach(validateTrackedInput)
    val duplicateNames = allFiles.groupBy(_.path.last).collect { case (name, files) if files.size > 1 => name }
    if (duplicateNames.nonEmpty)
      throw new IllegalArgumentException(
        s"npm project files have duplicate names: ${duplicateNames.toSeq.sorted.mkString(", ")}"
      )
    lockFiles.foreach(validateNpmLock)
    os.makeDir.all(root)
    allFiles.foreach(file => os.copy.over(file.path, root / file.path.last, createFolders = true))
    JavaScriptInstall(
      PathRef(root),
      projectFiles.map(file => PathRef(root / file.path.last)),
      lockFiles.map(file => PathRef(root / file.path.last))
    )
  }

  def ci(node: PathRef, npmCli: PathRef, cache: os.Path): JavaScriptCommand =
    NodeProcess.npm(
      node,
      npmCli,
      Seq("ci", "--ignore-scripts", "--no-audit", "--no-fund", "--cache", cache.toString)
    )

  def binary(
      node: PathRef,
      install: JavaScriptInstall,
      binary: PackageBinary,
      arguments: Seq[String]
  ): JavaScriptCommand = {
    val nodeModules = install.root.path / "node_modules"
    if (!Files.isDirectory(nodeModules.toNIO, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"JavaScript packages are not installed under ${install.root.path}")
    val candidates = packageDirectories(nodeModules).flatMap(resolvePackageBinary(install.root.path, _, binary))
    candidates.distinct match {
      case Seq(executable) => NodeProcess.runtime(node, executable.toString +: arguments)
      case Seq()           =>
        throw new IllegalArgumentException(
          s"Package binary '${binary.value}' is not installed under ${install.root.path}"
        )
      case ambiguous =>
        throw new IllegalArgumentException(
          s"Package binary '${binary.value}' is ambiguous under ${install.root.path}: ${ambiguous.mkString(", ")}"
        )
    }
  }

  private def packageDirectories(nodeModules: os.Path): Seq[os.Path] =
    children(nodeModules).filter(path => path.last != ".bin").flatMap { entry =>
      if (entry.last.startsWith("@") && Files.isDirectory(entry.toNIO)) children(entry)
      else Seq(entry)
    }

  private def children(directory: os.Path): Seq[os.Path] =
    Using.resource(Files.newDirectoryStream(directory.toNIO))(_.iterator().asScala.map(os.Path(_)).toSeq)

  private def resolvePackageBinary(
      installRoot: os.Path,
      packageDirectory: os.Path,
      binary: PackageBinary
  ): Seq[os.Path] = {
    val installReal = installRoot.toNIO.toRealPath()
    val packageReal = packageDirectory.toNIO.toRealPath()
    if (!packageReal.startsWith(installReal))
      throw new IllegalArgumentException(s"Installed npm package escapes its install root: $packageDirectory")
    val manifest = packageReal.resolve("package.json")
    if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) Seq.empty
    else {
      val json =
        try ujson.read(Files.readString(manifest))
        catch {
          case error: Exception =>
            throw new IllegalArgumentException(
              s"Invalid installed npm package manifest $manifest: ${error.getMessage}",
              error
            )
        }
      val matches = json.obj.get("bin").toSeq.flatMap {
        case ujson.Str(path) =>
          json.obj.get("name").map(_.str.split('/').last -> path).toSeq
        case value => value.obj.toSeq.map { case (name, path) => name -> path.str }
      }.collect { case (name, path) if name == binary.value => path }
      matches.map(path => safeInstalledBinary(installReal, packageReal, path))
    }
  }

  private def safeInstalledBinary(
      installRoot: java.nio.file.Path,
      packageRoot: java.nio.file.Path,
      value: String
  ): os.Path = {
    val relative =
      try os.RelPath(value)
      catch {
        case _: IllegalArgumentException =>
          throw new IllegalArgumentException(s"Installed npm package declares an unsafe binary path: '$value'")
      }
    if (
      value.isEmpty || value.contains('\\') || relative.ups > 0 || relative.segments.isEmpty ||
      relative.segments.exists(segment => segment == "." || segment == "..")
    ) throw new IllegalArgumentException(s"Installed npm package declares an unsafe binary path: '$value'")
    val target = packageRoot.resolve(relative.toString).toRealPath()
    if (!target.startsWith(installRoot) || !Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"Installed npm package binary escapes its install root: '$value'")
    os.Path(target)
  }

  private def validateTrackedInput(file: PathRef): Unit = {
    val path = file.path.toNIO
    if (Files.isSymbolicLink(path))
      throw new IllegalArgumentException(s"npm project input must not be a symbolic link: ${file.path}")
    else if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
      val stream = Files.walk(path)
      try
        stream.iterator().asScala.find(Files.isSymbolicLink) match {
          case Some(link) => throw new IllegalArgumentException(s"npm project input contains a symbolic link: $link")
          case None       => ()
        }
      finally stream.close()
    } else if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"npm project input must be a tracked file or directory: ${file.path}")
  }

  private def validateNpmLock(file: PathRef): Unit = {
    val name = file.path.last
    if (name != "package-lock.json" && name != "npm-shrinkwrap.json")
      throw new IllegalArgumentException(s"Unsupported npm lock file: ${file.path}")
    val lockVersion =
      try ujson.read(os.read(file.path)).obj.get("lockfileVersion").map(_.num.toInt)
      catch {
        case error: Exception =>
          throw new IllegalArgumentException(
            s"Invalid committed npm lock file ${file.path}: ${error.getMessage}",
            error
          )
      }
    if (!lockVersion.exists(_ >= 1))
      throw new IllegalArgumentException(s"Invalid committed npm lock file ${file.path}: missing lockfileVersion")
  }
}
