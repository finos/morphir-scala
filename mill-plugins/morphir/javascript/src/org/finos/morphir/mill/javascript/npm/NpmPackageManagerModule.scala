package org.finos.morphir.mill.javascript.npm

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileVisitResult,
  Files,
  LinkOption,
  OpenOption,
  Path as JPath,
  SimpleFileVisitor,
  StandardCopyOption,
  StandardOpenOption
}
import java.security.MessageDigest
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

import mill.*
import org.finos.morphir.mill.javascript.*
import org.finos.morphir.mill.javascript.node.{NodeProcess, NodeRuntimeModule}
import org.finos.morphir.mill.toolchain.{StorageSize, storageSize}
import upickle.default.ReadWriter

trait NpmPackageManagerModule extends JavaScriptPackageManagerModule {
  def runtime: NodeRuntimeModule

  def npmProjectPaths: Seq[os.Path] = Seq(moduleDir / "package.json")
  def npmLockPaths: Seq[os.Path]    = Seq(moduleDir / "package-lock.json")

  private[javascript] final def trackedProjectInputs: T[Seq[NpmProcess.TrackedNpmInput]] = Task.Input {
    NpmProcess.trackInputs(npmProjectPaths, NpmProcess.InputKind.Project)
  }

  private[javascript] final def trackedLockInputs: T[Seq[NpmProcess.TrackedNpmInput]] = Task.Input {
    NpmProcess.trackInputs(npmLockPaths, NpmProcess.InputKind.Lock)
  }

  final def projectFiles: T[Seq[PathRef]] = Task {
    trackedProjectInputs().map(_.pathRef)
  }

  final def lockFiles: T[Seq[PathRef]] = Task {
    trackedLockInputs().map(_.pathRef)
  }

  def packageJson: T[PathRef] = Task {
    projectFiles().find(_.path.last == "package.json").getOrElse {
      throw new IllegalArgumentException("npm project paths do not contain package.json")
    }
  }

  def packageLockJson: T[PathRef] = Task {
    lockFiles().find(_.path.last == "package-lock.json").getOrElse {
      throw new IllegalArgumentException("npm lock paths do not contain package-lock.json")
    }
  }

  def npmEnvironmentInputs: T[Seq[(String, String)]] = Task.Input {
    NpmProcess.retainedEnvironment(Task.env).toSeq.sorted
  }

  def install: T[JavaScriptInstall] = Task {
    val _             = runtime.runtimeVersion()
    val projectInputs = trackedProjectInputs()
    val lockInputs    = trackedLockInputs()
    val command       = NpmProcess.ci(runtime.runtimeExecutable(), runtime.npmCli(), Task.dest / "npm-cache")
    NpmProcess.install(
      Task.dest,
      projectInputs,
      lockInputs,
      command,
      npmEnvironmentInputs().toMap
    )
  }

  def packageManagerCommand(arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    NodeProcess.npm(runtime.runtimeExecutable(), runtime.npmCli(), arguments)
  }

  def packageBinaryCommand(binary: PackageBinary, arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    NpmProcess.binary(runtime.runtimeExecutable(), install(), binary, arguments)
  }
}

private[javascript] object NpmProcess {
  enum InputKind derives ReadWriter {
    case Project, Lock
  }

  final case class TrackedNpmInput(path: String, kind: InputKind, fingerprint: Vector[Int]) derives ReadWriter {
    def pathRef: PathRef = {
      val inputPath = os.Path(path, os.pwd)
      PathRef(
        inputPath,
        quick = false,
        sig = fingerprintSignature(fingerprintHex(fingerprint)),
        revalidate = PathRef.Revalidate.Never
      )
    }
  }

  final case class DiscoveryLimits(
      maxPackages: Option[Int] = Some(10000),
      maxDiscoveryEntries: Option[Int] = Some(20000),
      maxManifestBytes: Option[StorageSize] = Some(storageSize"1 MiB")
  )
  final case class InputLimits(
      maxEntries: Option[Int] = Some(10000),
      maxFileBytes: Option[StorageSize] = Some(storageSize"64 MiB"),
      maxTotalBytes: Option[StorageSize] = Some(storageSize"512 MiB")
  )

  private final case class VerifiedInput(original: PathRef, fingerprint: String)
  private final case class InputSnapshot(
      root: os.Path,
      projectNames: Seq[String],
      lockNames: Seq[String],
      originals: Seq[VerifiedInput],
      limits: InputLimits
  )

  private final class InputBudget(val limits: InputLimits) {
    private var entries = 0L
    private var bytes   = 0L

    def addEntry(path: JPath): Unit =
      entries = incrementOptionalCount(
        entries,
        limits.maxEntries,
        count =>
          new IllegalArgumentException(
            limits.maxEntries.fold(s"npm project input entry count exceeds $count at $path")(limit =>
              s"npm project input entry count limit $limit exceeded at $path"
            )
          )
      )

    def checkFileSize(path: JPath, size: Long): Unit = {
      limits.maxFileBytes.foreach { limit =>
        if (size > limit.toBytes)
          throw new IllegalArgumentException(
            s"npm project input file $path exceeds ${limit.show}"
          )
      }
      limits.maxTotalBytes.foreach { limit =>
        if (size > limit.toBytes - bytes)
          throw new IllegalArgumentException(
            s"npm project inputs exceed ${limit.show} total at $path"
          )
      }
    }

    def addBytes(path: JPath, count: Int): Unit = {
      bytes += count
      limits.maxTotalBytes.foreach { limit =>
        if (bytes > limit.toBytes)
          throw new IllegalArgumentException(
            s"npm project inputs exceed ${limit.show} total at $path"
          )
      }
    }
  }

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

  /**
   * Mill 1.2 does not follow links nested below a PathRef, but an initial directory-symlink path is still traversed.
   * Build npm inputs from a bounded no-follow fingerprint before constructing their PathRefs. Mill's action-cache
   * signature is an Int derived from serialized JSON, so retaining all digest bytes cannot make collisions impossible;
   * the byte-vector encoding avoids reducing the digest to PathRef's known 32-bit alias first.
   */
  def trackInputs(
      paths: Seq[os.Path],
      kind: InputKind,
      limits: InputLimits = InputLimits()
  ): Seq[TrackedNpmInput] = {
    validateInputLimits(limits)
    val budget = new InputBudget(limits)
    paths.map { path =>
      val fingerprint = scanInput(path.toNIO, None, budget)
      TrackedNpmInput(path.toString, kind, fingerprintBytes(fingerprint))
    }
  }

  def inputPathRefs(paths: Seq[os.Path], limits: InputLimits = InputLimits()): Seq[PathRef] =
    trackInputs(paths, InputKind.Project, limits).map(_.pathRef)

  def install(
      taskRoot: os.Path,
      projectInputs: Seq[TrackedNpmInput],
      lockInputs: Seq[TrackedNpmInput],
      command: JavaScriptCommand,
      environmentInputs: Map[String, String],
      limits: InputLimits = InputLimits(),
      beforeVerify: () => Unit = () => (),
      launch: (JavaScriptCommand, os.Path, Map[String, String]) => Unit = launchProcess
  ): JavaScriptInstall = {
    val snapshotRoot = taskRoot / "input-snapshot"
    val installRoot  = taskRoot / "install"
    try {
      val snapshot           = snapshotInputs(snapshotRoot, projectInputs, lockInputs, limits)
      val prepared           = prepareSnapshotInstall(installRoot, snapshot)
      val processEnvironment = environment(taskRoot / "process-state", environmentInputs)
      initialize(processEnvironment)
      beforeVerify()
      verifyOriginals(snapshot)
      launch(command, prepared.root.path, processEnvironment)
      removeOwned(snapshotRoot)
      prepared
    } catch {
      case error: Throwable =>
        removeOwned(snapshotRoot)
        removeOwned(snapshotRoot / os.up / "input-snapshot.staging")
        removeOwned(installRoot)
        removeOwned(installRoot / os.up / "install.staging")
        removeOwned(taskRoot / "process-state")
        removeOwned(taskRoot / "npm-cache")
        throw error
    }
  }

  def prepareInstall(
      root: os.Path,
      projectInputs: Seq[TrackedNpmInput],
      lockInputs: Seq[TrackedNpmInput],
      limits: InputLimits = InputLimits()
  ): JavaScriptInstall = {
    val snapshotRoot = root / os.up / "input-snapshot"
    try {
      val snapshot = snapshotInputs(snapshotRoot, projectInputs, lockInputs, limits)
      val prepared = prepareSnapshotInstall(root, snapshot)
      verifyOriginals(snapshot)
      prepared
    } catch {
      case error: Throwable =>
        removeOwned(root)
        throw error
    } finally removeOwned(snapshotRoot)
  }

  private def snapshotInputs(
      root: os.Path,
      projectInputs: Seq[TrackedNpmInput],
      lockInputs: Seq[TrackedNpmInput],
      limits: InputLimits
  ): InputSnapshot = {
    if (lockInputs.isEmpty)
      throw new IllegalArgumentException("npm install requires a committed npm lock file")
    validateInputLimits(limits)
    if (projectInputs.exists(_.kind != InputKind.Project) || lockInputs.exists(_.kind != InputKind.Lock))
      throw new IllegalArgumentException("npm tracked input kind does not match its project or lock role")
    val allInputs      = projectInputs ++ lockInputs
    val allFiles       = allInputs.map(_.pathRef)
    val duplicateNames = allFiles.groupBy(_.path.last).collect { case (name, files) if files.size > 1 => name }
    if (duplicateNames.nonEmpty)
      throw new IllegalArgumentException(
        s"npm project files have duplicate names: ${duplicateNames.toSeq.sorted.mkString(", ")}"
      )
    val staging = root / os.up / "input-snapshot.staging"
    removeOwned(staging)
    removeOwned(root)
    os.makeDir.all(staging)
    try {
      val baselineBudget = new InputBudget(limits)
      val baselines      = allInputs.map { tracked =>
        val input       = tracked.pathRef
        val fingerprint = scanInput(input.path.toNIO, None, baselineBudget)
        if (fingerprint != fingerprintHex(tracked.fingerprint) || fingerprintSignature(fingerprint) != input.sig)
          throw changedInput(input.path, "full fingerprint changed before its verified snapshot")
        input -> fingerprint
      }
      val copyBudget = new InputBudget(limits)
      val verified   = baselines.map { case (input, fingerprint) =>
        val confirmation = scanInput(input.path.toNIO, Some((staging / input.path.last).toNIO), copyBudget)
        if (confirmation != fingerprint || fingerprintSignature(confirmation) != input.sig)
          throw changedInput(input.path, "changed while creating its verified snapshot")
        VerifiedInput(input, fingerprint)
      }
      lockInputs.foreach(input => validateNpmLock(PathRef(staging / input.pathRef.path.last)))
      promote(staging, root)
      InputSnapshot(
        root,
        projectInputs.map(_.pathRef.path.last),
        lockInputs.map(_.pathRef.path.last),
        verified,
        limits
      )
    } catch {
      case error: Throwable =>
        removeOwned(staging)
        removeOwned(root)
        throw error
    }
  }

  private def prepareSnapshotInstall(root: os.Path, snapshot: InputSnapshot): JavaScriptInstall = {
    val staging = root / os.up / "install.staging"
    removeOwned(staging)
    removeOwned(root)
    os.makeDir.all(staging)
    try {
      val budget = new InputBudget(snapshot.limits)
      (snapshot.projectNames ++ snapshot.lockNames).foreach { name =>
        scanInput((snapshot.root / name).toNIO, Some((staging / name).toNIO), budget)
      }
      promote(staging, root)
      JavaScriptInstall(
        PathRef(root),
        snapshot.projectNames.map(name => PathRef(root / name)),
        snapshot.lockNames.map(name => PathRef(root / name))
      )
    } catch {
      case error: Throwable =>
        removeOwned(staging)
        removeOwned(root)
        throw error
    }
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
      arguments: Seq[String],
      limits: DiscoveryLimits = DiscoveryLimits()
  ): JavaScriptCommand = {
    validateOptionalCount(limits.maxPackages, "installed package count")
    validateOptionalCount(limits.maxDiscoveryEntries, "installed package discovery entry count")
    validateOptionalSize(limits.maxManifestBytes, "installed package manifest bytes")
    val installPath = install.root.path.toNIO
    if (Files.isSymbolicLink(installPath))
      throw new IllegalArgumentException(s"JavaScript install root must not be a symbolic link: $installPath")
    if (!Files.isDirectory(installPath, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"JavaScript install root is not a directory: $installPath")
    val installReal = installPath.toRealPath()
    val nodeModules = install.root.path / "node_modules"
    if (!Files.isDirectory(nodeModules.toNIO, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"JavaScript packages are not installed under ${install.root.path}")
    val nodeModulesReal = requireContainedDirectory(installReal, nodeModules.toNIO, "npm node_modules directory")
    val candidates      = packageDirectories(installReal, nodeModulesReal, limits)
      .flatMap(resolvePackageBinary(installReal, _, binary, limits.maxManifestBytes))
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

  private def packageDirectories(
      installRoot: java.nio.file.Path,
      nodeModules: java.nio.file.Path,
      limits: DiscoveryLimits
  ): Seq[java.nio.file.Path] = {
    val packages = ArrayBuffer.empty[java.nio.file.Path]
    var entries  = 0L

    def countEntry(path: java.nio.file.Path): Unit =
      entries = incrementOptionalCount(
        entries,
        limits.maxDiscoveryEntries,
        count =>
          new IllegalArgumentException(
            limits.maxDiscoveryEntries.fold(
              s"Installed npm package discovery entry count exceeds $count at $path"
            )(limit => s"Installed npm package discovery entry count limit $limit exceeded at $path")
          )
      )

    def addPackage(path: java.nio.file.Path): Unit = {
      limits.maxPackages.foreach { limit =>
        if (packages.size >= limit)
          throw new IllegalArgumentException(
            s"Installed npm package count limit $limit exceeded under $nodeModules"
          )
      }
      packages += path
    }

    foreachChild(nodeModules) { entry =>
      countEntry(entry)
      if (entry.getFileName.toString != ".bin") {
        if (entry.getFileName.toString.startsWith("@")) {
          if (Files.isSymbolicLink(entry))
            throw new IllegalArgumentException(s"Installed npm package scope must not be a symbolic link: $entry")
          if (Files.isDirectory(entry, LinkOption.NOFOLLOW_LINKS)) {
            val scope = requireContainedDirectory(installRoot, entry, "Installed npm package scope")
            foreachChild(scope) { packageEntry =>
              countEntry(packageEntry)
              if (Files.isSymbolicLink(packageEntry) || Files.isDirectory(packageEntry, LinkOption.NOFOLLOW_LINKS))
                addPackage(packageEntry)
            }
          }
        } else if (Files.isSymbolicLink(entry) || Files.isDirectory(entry, LinkOption.NOFOLLOW_LINKS))
          addPackage(entry)
      }
    }
    packages.toSeq
  }

  private def foreachChild(directory: java.nio.file.Path)(f: java.nio.file.Path => Unit): Unit =
    Using.resource(Files.newDirectoryStream(directory))(_.iterator().asScala.foreach(f))

  private def resolvePackageBinary(
      installRoot: java.nio.file.Path,
      packageDirectory: java.nio.file.Path,
      binary: PackageBinary,
      maxManifestBytes: Option[StorageSize]
  ): Seq[os.Path] = {
    val packageReal = requireContainedPackageDirectory(installRoot, packageDirectory)
    val manifest    = packageReal.resolve("package.json")
    if (Files.isSymbolicLink(manifest))
      throw invalidManifest(manifest, "$", "manifest must not be a symbolic link")
    else if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) Seq.empty
    else {
      val json = try ujson.read(readBoundedManifest(manifest, maxManifestBytes))
      catch {
        case error: IllegalArgumentException
            if error.getMessage.startsWith("Invalid installed npm package manifest") =>
          throw error
        case error: Exception => throw invalidManifest(manifest, "$", error.getMessage, error)
      }
      val fields = json match {
        case value: ujson.Obj => value.obj
        case _                => throw invalidManifest(manifest, "$", "expected an object")
      }
      val packageName = fields.get("name").map {
        case ujson.Str(value) if validPackageName(value) => value.split('/').last
        case ujson.Str(_)                                =>
          throw invalidManifest(manifest, "$.name", "expected an npm package name")
        case _ => throw invalidManifest(manifest, "$.name", "expected a string")
      }
      val declared = fields.get("bin").toSeq.flatMap {
        case ujson.Str(path) =>
          packageName match {
            case Some(name) => Seq(name -> path)
            case None       => throw invalidManifest(manifest, "$.name", "required when bin is a string")
          }
        case value: ujson.Obj =>
          value.obj.toSeq.map {
            case (name, ujson.Str(path)) => name -> path
            case (name, _)               =>
              throw invalidManifest(manifest, s"$$.bin.$name", "expected a string")
          }
        case _ => throw invalidManifest(manifest, "$.bin", "expected a string or object")
      }
      declared.collect { case (name, path) if name == binary.value => path }
        .map(path => safeInstalledBinary(installRoot, packageReal, path))
    }
  }

  private def validPackageName(value: String): Boolean =
    if (value.startsWith("@")) {
      val parts = value.split("/", -1)
      parts.length == 2 && parts(0).length > 1 && parts(1).nonEmpty && !parts(1).contains('\\')
    } else value.nonEmpty && !value.contains('/') && !value.contains('\\')

  private def readBoundedManifest(manifest: java.nio.file.Path, maxBytes: Option[StorageSize]): String = {
    val attributes = Files.readAttributes(manifest, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
    if (!attributes.isRegularFile)
      throw invalidManifest(manifest, "$", "manifest is not a regular file")
    maxBytes.foreach { limit =>
      if (attributes.size() > limit.toBytes)
        throw invalidManifest(manifest, "$", s"manifest exceeds ${limit.show}")
    }
    val options = Set[OpenOption](StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS).asJava
    Using.resource(Files.newByteChannel(manifest, options)) { channel =>
      val boundedSize = maxBytes.fold(attributes.size())(limit => math.min(attributes.size(), limit.toBytes))
      val output      = new ByteArrayOutputStream(math.min(boundedSize, 8192L).toInt)
      val buffer      = ByteBuffer.allocate(8192)
      var total       = 0L
      var read        = channel.read(buffer)
      while (read >= 0) {
        if (read > 0) {
          total += read
          maxBytes.foreach { limit =>
            if (total > limit.toBytes)
              throw invalidManifest(manifest, "$", s"manifest exceeds ${limit.show}")
          }
          output.write(buffer.array(), 0, read)
          buffer.clear()
        }
        read = channel.read(buffer)
      }
      output.toString(StandardCharsets.UTF_8)
    }
  }

  private def validateOptionalCount(value: Option[Int], description: String): Unit =
    value.foreach(limit => requirePositive(limit.toLong, description))

  private[javascript] def incrementOptionalCount(
      current: Long,
      limit: Option[Int],
      onExceeded: Long => IllegalArgumentException
  ): Long = {
    if (current == Long.MaxValue) throw onExceeded(current)
    val updated = current + 1L
    limit.foreach(maximum => if (updated > maximum.toLong) throw onExceeded(updated))
    updated
  }

  private def validateOptionalSize(value: Option[StorageSize], description: String): Unit =
    value.foreach(limit => requirePositive(limit.toBytes, description))

  private def requirePositive(value: Long, description: String): Unit =
    if (value <= 0) throw new IllegalArgumentException(s"npm $description limit must be positive: $value")

  private def validateInputLimits(limits: InputLimits): Unit = {
    validateOptionalCount(limits.maxEntries, "project input entry count")
    validateOptionalSize(limits.maxFileBytes, "project input file bytes")
    validateOptionalSize(limits.maxTotalBytes, "project input total bytes")
  }

  private def scanInput(
      source: JPath,
      destination: Option[JPath],
      budget: InputBudget
  ): String = {
    if (!Files.exists(source, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"npm project input does not exist: $source")
    val records = ArrayBuffer.empty[String]

    def relativeName(path: JPath): String = {
      val relative = source.relativize(path).iterator().asScala.map(_.toString).mkString("/")
      if (relative.isEmpty) source.getFileName.toString else s"${source.getFileName}/$relative"
    }

    def destinationPath(path: JPath): Option[JPath] =
      destination.map(_.resolve(source.relativize(path)))

    Files.walkFileTree(
      source,
      new SimpleFileVisitor[JPath] {
        override def preVisitDirectory(directory: JPath, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink || Files.isSymbolicLink(directory))
            throw new IllegalArgumentException(s"npm project input contains a symbolic link: $directory")
          budget.addEntry(directory)
          destinationPath(directory).foreach(Files.createDirectories(_))
          records += s"D:${relativeName(directory)}"
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: JPath, attributes: BasicFileAttributes): FileVisitResult = {
          if (attributes.isSymbolicLink || Files.isSymbolicLink(file))
            throw new IllegalArgumentException(s"npm project input contains a symbolic link: $file")
          if (!attributes.isRegularFile)
            throw new IllegalArgumentException(s"npm project input must contain only regular files: $file")
          budget.addEntry(file)
          budget.checkFileSize(file, attributes.size())
          val digest       = MessageDigest.getInstance("SHA-256")
          val readOptions  = Set[OpenOption](StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS).asJava
          val writeOptions = Set[OpenOption](StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE).asJava
          val input        = Files.newByteChannel(file, readOptions)
          val output       = destinationPath(file).map { target =>
            Option(target.getParent).foreach(Files.createDirectories(_))
            Files.newByteChannel(target, writeOptions)
          }
          try {
            val buffer   = ByteBuffer.allocate(8192)
            var fileSize = 0L
            var read     = input.read(buffer)
            while (read >= 0) {
              if (read > 0) {
                fileSize += read
                budget.limits.maxFileBytes.foreach { limit =>
                  if (fileSize > limit.toBytes)
                    throw new IllegalArgumentException(
                      s"npm project input file $file exceeds ${limit.show}"
                    )
                }
                budget.addBytes(file, read)
                buffer.flip()
                digest.update(buffer.asReadOnlyBuffer())
                output.foreach(channel => while (buffer.hasRemaining) channel.write(buffer))
                buffer.clear()
              }
              read = input.read(buffer)
            }
          } finally {
            output.foreach(_.close())
            input.close()
          }
          val after       = Files.readAttributes(file, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
          val sameFileKey =
            attributes.fileKey() == null || after.fileKey() == null || attributes.fileKey() == after.fileKey()
          if (
            !after.isRegularFile || attributes.size() != after.size() ||
            attributes.lastModifiedTime() != after.lastModifiedTime() || !sameFileKey
          ) throw changedInput(os.Path(file), "changed while creating its verified snapshot")
          records += s"F:${relativeName(file)}:${after.size()}:${hex(digest.digest())}"
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(file: JPath, error: java.io.IOException): FileVisitResult =
          throw new IllegalArgumentException(s"Unable to read npm project input $file: ${error.getMessage}", error)
      }
    )
    val digest = MessageDigest.getInstance("SHA-256")
    records.sorted.foreach { record =>
      digest.update(record.getBytes(StandardCharsets.UTF_8))
      digest.update(0.toByte)
    }
    hex(digest.digest())
  }

  private def verifyOriginals(snapshot: InputSnapshot): Unit = {
    val budget = new InputBudget(snapshot.limits)
    snapshot.originals.foreach { verified =>
      try {
        val fingerprint = scanInput(verified.original.path.toNIO, None, budget)
        if (
          fingerprint != verified.fingerprint ||
          fingerprintSignature(fingerprint) != verified.original.sig
        )
          throw changedInput(verified.original.path, "content fingerprint changed")
      } catch {
        case error: IllegalArgumentException
            if error.getMessage.startsWith("npm project input changed after its verified snapshot") =>
          throw error
        case error: Exception =>
          throw changedInput(verified.original.path, error.getMessage, error)
      }
    }
  }

  private def changedInput(path: os.Path, detail: String, cause: Throwable = null): IllegalArgumentException =
    new IllegalArgumentException(
      s"npm project input changed after its verified snapshot: $path ($detail)",
      cause
    )

  private def hex(bytes: Array[Byte]): String = bytes.iterator.map(byte => f"${byte & 0xff}%02x").mkString

  private def fingerprintSignature(fingerprint: String): Int =
    java.util.Arrays.hashCode(fingerprint.getBytes(StandardCharsets.US_ASCII))

  private def fingerprintBytes(fingerprint: String): Vector[Int] =
    fingerprint.grouped(2).map(Integer.parseInt(_, 16)).toVector

  private def fingerprintHex(fingerprint: Vector[Int]): String =
    fingerprint.iterator.map(byte => f"$byte%02x").mkString

  private def promote(staging: os.Path, destination: os.Path): Unit =
    try Files.move(staging.toNIO, destination.toNIO, StandardCopyOption.ATOMIC_MOVE)
    catch {
      case _: AtomicMoveNotSupportedException => Files.move(staging.toNIO, destination.toNIO)
    }

  private def removeOwned(path: os.Path): Unit =
    if (Files.isSymbolicLink(path.toNIO)) Files.deleteIfExists(path.toNIO)
    else if (Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS)) os.remove.all(path)

  private def launchProcess(
      command: JavaScriptCommand,
      cwd: os.Path,
      processEnvironment: Map[String, String]
  ): Unit = {
    val _ = os.proc(command.executable.path.toString +: command.arguments)
      .call(cwd = cwd, env = processEnvironment, propagateEnv = false)
  }

  private def invalidManifest(
      manifest: java.nio.file.Path,
      jsonPath: String,
      detail: String,
      cause: Throwable = null
  ): IllegalArgumentException =
    new IllegalArgumentException(s"Invalid installed npm package manifest $manifest at $jsonPath: $detail", cause)

  private def requireContainedDirectory(
      installRoot: java.nio.file.Path,
      directory: java.nio.file.Path,
      description: String
  ): java.nio.file.Path = {
    if (Files.isSymbolicLink(directory))
      throw new IllegalArgumentException(s"$description must not be a symbolic link: $directory")
    if (!Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"$description is not a directory: $directory")
    val real = directory.toRealPath()
    if (!real.startsWith(installRoot))
      throw new IllegalArgumentException(s"$description escapes its install root: $directory")
    real
  }

  private def requireContainedPackageDirectory(
      installRoot: java.nio.file.Path,
      directory: java.nio.file.Path
  ): java.nio.file.Path = {
    val real = try directory.toRealPath()
    catch {
      case error: Exception =>
        throw new IllegalArgumentException(s"Installed npm package is not a readable directory: $directory", error)
    }
    if (!real.startsWith(installRoot))
      throw new IllegalArgumentException(s"Installed npm package escapes its install root: $directory")
    if (!Files.isDirectory(real, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(s"Installed npm package is not a directory: $directory")
    real
  }

  private def safeInstalledBinary(
      installRoot: java.nio.file.Path,
      packageRoot: java.nio.file.Path,
      value: String
  ): os.Path = {
    val windowsDrivePrefixed = value.length >= 2 && value.charAt(0).isLetter && value.charAt(1) == ':'
    if (value.startsWith("/") || value.startsWith("\\") || windowsDrivePrefixed)
      throw new IllegalArgumentException(s"Installed npm package binary path must be relative: '$value'")
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
    val target = try packageRoot.resolve(relative.toString).toRealPath()
    catch {
      case error: Exception =>
        throw new IllegalArgumentException(
          s"Installed npm package binary target is not readable: '$value' under $packageRoot",
          error
        )
    }
    if (!target.startsWith(installRoot) || !Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS))
      throw new IllegalArgumentException(
        s"Installed npm package binary target is not a contained regular file: '$value' under $packageRoot"
      )
    os.Path(target)
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
