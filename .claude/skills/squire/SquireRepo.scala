//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala, SquireCellar.scala]

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, LinkOption, Path as JavaPath, SimpleFileVisitor}
import kyo.*

final case class ReferenceManifest(repos: List[ReferenceRepo] = Nil) derives Schema

final case class ReferenceRepo(
    name: String,
    org: Option[String],
    path: String,
    added: String,
    strategy: String,
    url: Option[String] = None,
    source: Option[String] = None,
    ref: Option[String] = None,
    commit: Option[String] = None,
    depth: Option[Int] = None,
    sparse: List[String] = Nil
) derives Schema

final case class ReferenceAdd(
    urlOrPath: String,
    name: Option[String] = None,
    ref: Option[String] = None,
    strategy: String = "clone",
    depth: Option[Int] = None,
    full: Boolean = false,
    sparse: List[String] = Nil
) derives CanEqual

final case class ReferenceStatusReport(output: String, exitCode: Int) derives CanEqual

enum ReferencePathKind:
  case Missing
  case Directory(realPath: Path)
  case Symlink(target: Option[Path])

final case class ManagedReferencePath(path: Path, kind: ReferencePathKind)

final case class ReferenceEntryStatus(
    repo: ReferenceRepo,
    path: Path,
    diskLabel: String,
    healthy: Boolean,
    current: Option[String] = None,
    branch: Option[String] = None,
    dirty: Boolean = false,
    problem: Option[String] = None
)

object SquireRepo:
  def nameFrom(urlOrPath: String): String =
    val trimmed = urlOrPath.stripSuffix("/")
    val leaf    = trimmed.lastIndexOf('/') match
      case -1    => trimmed.split(':').lastOption.getOrElse(trimmed)
      case index => trimmed.substring(index + 1)
    leaf.stripSuffix(".git")

  def orgFrom(urlOrPath: String): Option[String] =
    val trimmed = urlOrPath.stripSuffix("/").stripSuffix(".git")
    if trimmed.startsWith("git@") then
      val remotePath = trimmed.drop(trimmed.indexOf(':') + 1)
      ownerFromSegments(remotePath.split('/').toList)
    else if trimmed.contains("://") then
      val withoutScheme = trimmed.substring(trimmed.indexOf("://") + 3)
      ownerFromSegments(withoutScheme.split('/').toList.drop(1))
    else
      Option(java.nio.file.Path.of(urlOrPath).toAbsolutePath.normalize.getParent)
        .flatMap(parent => Option(parent.getFileName))
        .map(_.toString)
        .filter(_.nonEmpty)

  def validate(options: ReferenceAdd, manifest: ReferenceManifest): Result[SquireError, Unit] =
    val name       = options.name.getOrElse(nameFrom(options.urlOrPath))
    val components = List(name) ++ orgFrom(options.urlOrPath).toList ++
      (if options.strategy == "worktree" then List(sourceRepositoryName(options.urlOrPath)) else Nil)
    if components.exists(component => !validComponent(component)) then
      failure("repository names, organizations, and source names must be single filename segments")
    else if manifest.repos.exists(_.name == name) then failure(s"repo '$name' already in manifest")
    else if options.full && options.depth.nonEmpty then failure("--full and --depth are mutually exclusive")
    else if options.sparse.nonEmpty && options.strategy != "clone" then
      failure(s"--sparse applies to the clone strategy only, not ${options.strategy}")
    else if !Set("clone", "symlink", "worktree").contains(options.strategy) then
      failure(s"unknown reference repository strategy: ${options.strategy}")
    else if options.strategy == "worktree" && options.ref.isEmpty then
      failure("--ref is required for worktree strategy")
    else Result.Success(())

  def ghAuthenticated(result: ProcessResult): Boolean =
    val combined = result.stdout + result.stderr
    combined.contains("Logged in") || combined.contains("✓")

  def cloneRequest(options: ReferenceAdd, dest: Path, useGh: Boolean): ProcessRequest =
    val flags = cloneFlags(options)
    if useGh then
      ProcessRequest(Chunk("gh", "repo", "clone", options.urlOrPath, dest.toString) ++
        (if flags.isEmpty then Chunk.empty else Chunk("--") ++ flags))
    else ProcessRequest(Chunk("git", "clone") ++ flags ++ Chunk(options.urlOrPath, dest.toString))

  def sparseRequest(dest: Path, sparse: List[String]): ProcessRequest =
    ProcessRequest(Chunk("git", "-C", dest.toString, "sparse-checkout", "set") ++ Chunk.from(sparse))

  def loadManifest(root: Path): ReferenceManifest < (Sync & Abort[SquireError]) =
    fromResultEffect(Sync.defer(readManifest(root)))

  def saveManifest(root: Path, manifest: ReferenceManifest): Unit < (Sync & Abort[SquireError]) =
    fromResultEffect(Sync.defer(writeManifest(root, manifest)))

  def add(
      options: ReferenceAdd,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform
  ): ReferenceRepo < (Async & Sync & Abort[SquireError]) =
    for
      manifest <- loadManifest(root)
      _        <- fromResult(validate(options, manifest))
      refs = root / ".refs"
      _ <- Sync.defer(Files.createDirectories(refs.toJava))
      name         = options.name.filter(_.nonEmpty).getOrElse(nameFrom(options.urlOrPath))
      org          = orgFrom(options.urlOrPath)
      relativePath = relativePathFor(options, name, org)
      lexicalDest  = refs / relativePath
      dest     <- fromResult(SquirePaths.resolveUnder(lexicalDest, refs))
      _        <- ensureDestinationAbsent(dest)
      _        <- Sync.defer(Files.createDirectories(dest.parent.get.toJava))
      metadata <- addByStrategy(options, dest, runner, platform)
      entry = ReferenceRepo(
        name = name,
        org = org,
        path = relativePath,
        added = platform.now.toString,
        strategy = options.strategy,
        url = if options.strategy == "clone" then Some(options.urlOrPath) else None,
        source = metadata.source,
        ref = metadata.ref,
        commit = metadata.commit,
        depth = metadata.depth,
        sparse = if options.strategy == "clone" then options.sparse else Nil
      )
      _ <- saveManifest(root, ReferenceManifest(manifest.repos :+ entry))
    yield entry

  def list(root: Path, asJson: Boolean, runner: ProcessRunner): String < (Async & Sync & Abort[SquireError]) =
    loadManifest(root).flatMap { manifest =>
      if asJson then SquireJson.encode(manifest) + "\n"
      else if manifest.repos.isEmpty then "No reference repos. Use reference repo add to add one.\n"
      else
        statusEntries(root, manifest.repos, runner).map { statuses =>
          val header = f"${"NAME"}%-20s ${"PATH"}%-28s ${"STRATEGY"}%-10s ${"REF"}%-20s STATUS\n" + ("-" * 100) + "\n"
          val rows   = statuses.map { status =>
            val repo = status.repo
            f"${repo.name}%-20s ${repo.path}%-28s ${repo.strategy}%-10s ${repo.ref.getOrElse("").take(20)}%-20s ${status.diskLabel}"
          }
          header + rows.mkString("\n") + "\n"
        }
    }

  def status(
      root: Path,
      filterName: Option[String],
      runner: ProcessRunner
  ): ReferenceStatusReport < (Async & Sync & Abort[SquireError]) =
    loadManifest(root).flatMap { manifest =>
      if manifest.repos.isEmpty then ReferenceStatusReport("No reference repos in manifest.\n", 0)
      else
        val selected = filterName match
          case Some(name) => manifest.repos.filter(_.name == name)
          case None       => manifest.repos
        if selected.isEmpty then ReferenceStatusReport(s"ERROR: '${filterName.getOrElse("")}' not in manifest\n", 1)
        else
          statusEntries(root, selected, runner).map { entries =>
            val output = entries.map(entry => s"\n[${entry.repo.name}]\n${renderDetailedStatus(entry)}\n").mkString
            ReferenceStatusReport(output, if entries.forall(_.healthy) then 0 else 1)
          }
    }

  def remove(
      name: String,
      keepFiles: Boolean,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform
  ): Unit < (Async & Sync & Abort[SquireError]) =
    loadManifest(root).flatMap { manifest =>
      manifest.repos.find(_.name == name) match
        case None        => Abort.fail(SquireError.Failure("repo", s"'$name' not in manifest"))
        case Some(entry) =>
          val refs = root / ".refs"
          if keepFiles then saveManifest(root, ReferenceManifest(manifest.repos.filterNot(_.name == name)))
          else
            fromResult(managedReferencePath(entry, refs)).flatMap { managed =>
              removeFiles(entry, managed, runner).flatMap { _ =>
                pruneEmptyParents(managed.path.parent.get, refs).flatMap { _ =>
                  saveManifest(root, ReferenceManifest(manifest.repos.filterNot(_.name == name)))
                }
              }
            }
    }

  private final case class AddMetadata(
      source: Option[String],
      ref: Option[String],
      commit: Option[String],
      depth: Option[Int]
  )

  private def addByStrategy(
      options: ReferenceAdd,
      dest: Path,
      runner: ProcessRunner,
      platform: SquirePlatform
  ): AddMetadata < (Async & Sync & Abort[SquireError]) =
    options.strategy match
      case "clone"   => addClone(options, dest, runner, platform)
      case "symlink" =>
        val source = java.nio.file.Path.of(options.urlOrPath).toRealPath()
        Sync.defer(Files.createSymbolicLink(dest.toJava, source)).flatMap { _ =>
          repositoryMetadata(Path(source.toString), runner).map((ref, commit) =>
            AddMetadata(Some(source.toString), ref, commit, None)
          )
        }
      case "worktree" =>
        val source  = java.nio.file.Path.of(options.urlOrPath).toRealPath()
        val request = ProcessRequest(
          Chunk(
            "git",
            "-C",
            source.toString,
            "worktree",
            "add",
            dest.toJava.toAbsolutePath.normalize.toString,
            options.ref.get
          )
        )
        runChecked(runner, request).flatMap { _ =>
          gitValue(dest, runner, Chunk("rev-parse", "HEAD")).map(commit =>
            AddMetadata(Some(source.toString), options.ref, commit, None)
          )
        }
      case other => Abort.fail(SquireError.Failure("repo", s"unknown reference repository strategy: $other"))

  private def addClone(
      options: ReferenceAdd,
      dest: Path,
      runner: ProcessRunner,
      platform: SquirePlatform
  ): AddMetadata < (Async & Abort[SquireError]) =
    val useGh: Boolean < (Async & Abort[SquireError]) =
      if isGithub(options.urlOrPath) then
        platform.findExecutable("gh") match
          case Present(_) => runner.run(ProcessRequest(Chunk("gh", "auth", "status"))).map(ghAuthenticated)
          case Absent     => false
      else false
    useGh.flatMap { authenticated =>
      runChecked(runner, cloneRequest(options, dest, authenticated)).flatMap { _ =>
        val sparseSetup: Unit < (Async & Abort[SquireError]) =
          if options.sparse.nonEmpty then runChecked(runner, sparseRequest(dest, options.sparse)).unit else ()
        sparseSetup.flatMap { _ =>
          repositoryMetadata(dest, runner).map { case (ref, commit) =>
            AddMetadata(None, ref, commit, effectiveDepth(options))
          }
        }
      }
    }

  private def repositoryMetadata(
      path: Path,
      runner: ProcessRunner
  ): (Option[String], Option[String]) < (Async & Abort[SquireError]) =
    gitValue(path, runner, Chunk("symbolic-ref", "--short", "HEAD")).flatMap { branch =>
      gitValue(path, runner, Chunk("rev-parse", "HEAD")).map { commit =>
        (branch.orElse(commit), commit)
      }
    }

  private def gitValue(
      path: Path,
      runner: ProcessRunner,
      arguments: Chunk[String]
  ): Option[String] < (Async & Abort[SquireError]) =
    runner.run(ProcessRequest(Chunk("git", "-C", path.toString) ++ arguments)).map { result =>
      if result.exitCode == 0 then Option(result.stdout.trim).filter(_.nonEmpty) else None
    }

  private def statusEntries(
      root: Path,
      repos: List[ReferenceRepo],
      runner: ProcessRunner
  ): List[ReferenceEntryStatus] < (Async & Sync & Abort[SquireError]) =
    repos match
      case Nil          => Nil
      case repo :: tail =>
        statusEntry(root, repo, runner).flatMap { head =>
          statusEntries(root, tail, runner).map(head :: _)
        }

  private def statusEntry(
      root: Path,
      repo: ReferenceRepo,
      runner: ProcessRunner
  ): ReferenceEntryStatus < (Async & Sync & Abort[SquireError]) =
    val refs = root / ".refs"
    Sync.defer(managedReferencePath(repo, refs)).flatMap {
      case Result.Failure(error) =>
        ReferenceEntryStatus(
          repo,
          refs / repo.path,
          "INVALID_PATH",
          healthy = false,
          problem = Some(error.getMessage)
        )
      case Result.Success(managed) =>
        managed.kind match
          case ReferencePathKind.Missing =>
            ReferenceEntryStatus(repo, managed.path, "MISSING", healthy = false, problem = Some("not found on disk"))
          case ReferencePathKind.Symlink(None) =>
            ReferenceEntryStatus(
              repo,
              managed.path,
              "BROKEN_SYMLINK",
              healthy = false,
              problem = Some("target does not exist")
            )
          case ReferencePathKind.Symlink(Some(target)) =>
            gitEntryStatus(repo, managed.path, s"symlink → $target", runner)
          case ReferencePathKind.Directory(realPath) =>
            gitEntryStatus(repo, realPath, "", runner)
    }

  private def gitEntryStatus(
      repo: ReferenceRepo,
      path: Path,
      fixedDiskLabel: String,
      runner: ProcessRunner
  ): ReferenceEntryStatus < (Async & Abort[SquireError]) =
    runner.run(ProcessRequest(Chunk("git", "-C", path.toString, "rev-parse", "HEAD"))).flatMap { headResult =>
      val current = Option(headResult.stdout.trim).filter(_.nonEmpty)
      if headResult.exitCode != 0 || current.isEmpty then
        ReferenceEntryStatus(
          repo,
          path,
          if fixedDiskLabel.nonEmpty then fixedDiskLabel else "DIR_NO_GIT",
          healthy = false,
          problem = Some("DIR_NO_GIT — could not read Git HEAD")
        )
      else
        runner.run(ProcessRequest(Chunk("git", "-C", path.toString, "symbolic-ref", "--short", "HEAD"))).flatMap {
          branchResult =>
            val branch =
              if branchResult.exitCode == 0 then Option(branchResult.stdout.trim).filter(_.nonEmpty) else None
            runner.run(ProcessRequest(Chunk("git", "-C", path.toString, "status", "--porcelain"))).map {
              statusResult =>
                val dirty  = statusResult.exitCode == 0 && statusResult.stdout.trim.nonEmpty
                val drift  = current != repo.commit
                val sparse = if repo.sparse.nonEmpty then " [sparse]" else ""
                val label  =
                  if fixedDiskLabel.nonEmpty then fixedDiskLabel
                  else if statusResult.exitCode != 0 then "GIT_ERROR"
                  else if drift then
                    s"MODIFIED (was ${repo.commit.getOrElse("?").take(8)}, now ${current.get.take(8)})$sparse"
                  else s"OK (${current.get.take(8)})$sparse"
                ReferenceEntryStatus(
                  repo,
                  path,
                  label,
                  healthy = statusResult.exitCode == 0 && !drift && !dirty,
                  current = current,
                  branch = branch,
                  dirty = dirty,
                  problem =
                    if statusResult.exitCode != 0 then Some("GIT_ERROR — git status failed")
                    else if drift then Some("DRIFT — manifest commit differs from current HEAD")
                    else None
                )
            }
        }
    }

  private def renderDetailedStatus(status: ReferenceEntryStatus): String =
    val repo   = status.repo
    val prefix = List(
      s"  name:     ${repo.name}",
      s"  path:     ${status.path}",
      s"  strategy: ${repo.strategy}"
    ) ++ repo.url.map(value => s"  url:      $value") ++
      repo.source.map(value => s"  source:   $value") ++
      (if repo.sparse.nonEmpty then List(s"  sparse:   ${repo.sparse.mkString(" ")}") else Nil) ++
      List(
        s"  ref:      ${repo.ref.getOrElse("?")}",
        s"  recorded: ${repo.commit.getOrElse("?")}",
        s"  added:    ${repo.added}"
      )
    val state = List(s"  disk:     ${status.diskLabel}") ++
      status.current.map(value => s"  current:  $value (${status.branch.getOrElse("(detached)")})") ++
      status.problem.toList.map(value => s"  $value") ++
      (if status.dirty then List("  DIRTY — uncommitted changes present") else Nil) ++
      (if status.healthy then List("  in sync with manifest") else Nil)
    (prefix ++ state).mkString("\n")

  private def removeFiles(
      entry: ReferenceRepo,
      managed: ManagedReferencePath,
      runner: ProcessRunner
  ): Unit < (Async & Sync & Abort[SquireError]) =
    managed.kind match
      case ReferencePathKind.Missing             => ()
      case ReferencePathKind.Symlink(_)          => Sync.defer(Files.delete(managed.path.toJava))
      case ReferencePathKind.Directory(realPath) =>
        entry.strategy match
          case "worktree" =>
            entry.source match
              case Some(source) if Files.exists(java.nio.file.Path.of(source)) =>
                runChecked(
                  runner,
                  ProcessRequest(Chunk("git", "-C", source, "worktree", "remove", "--force", realPath.toString))
                ).unit
              case _ => deleteTree(realPath)
          case _ => deleteTree(realPath)

  private def deleteTree(path: Path): Unit < Sync =
    Sync.defer {
      Files.walkFileTree(
        path.toJava,
        new SimpleFileVisitor[JavaPath]:
          override def visitFile(file: JavaPath, attrs: BasicFileAttributes): FileVisitResult =
            Files.delete(file)
            FileVisitResult.CONTINUE

          override def postVisitDirectory(directory: JavaPath, error: java.io.IOException): FileVisitResult =
            if error != null then throw error
            Files.delete(directory)
            FileVisitResult.CONTINUE
      )
    }

  private def pruneEmptyParents(parent: Path, refs: Path): Unit < Sync =
    Sync.defer {
      val base      = refs.toJava.toAbsolutePath.normalize
      var current   = parent.toJava.toAbsolutePath.normalize
      var keepGoing = current != base && current.startsWith(base)
      while keepGoing do
        if Files.isSymbolicLink(current) || !Files.isDirectory(current, LinkOption.NOFOLLOW_LINKS) ||
          !directoryEmpty(current)
        then
          keepGoing = false
        else
          Files.delete(current)
          current = current.getParent
          keepGoing = current != null && current != base && current.startsWith(base)
    }

  private def directoryEmpty(path: JavaPath): Boolean =
    val entries = Files.list(path)
    try !entries.findFirst().isPresent
    finally entries.close()

  private def runChecked(runner: ProcessRunner, request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    runner.run(request).flatMap { result =>
      if result.exitCode == 0 then result
      else
        Abort.fail(
          SquireError.Failure(
            "repo",
            s"external command failed with exit ${result.exitCode}",
            Option(result.stderr.trim).filter(_.nonEmpty) match
              case Some(detail) => Present(detail)
              case None         => Absent
          )
        )
    }

  private def readManifest(root: Path): Result[SquireError, ReferenceManifest] =
    val path = root / ".refs" / "manifest.json"
    if !Files.exists(path.toJava) then Result.Success(ReferenceManifest())
    else
      try
        SquireJson.decode[ReferenceManifest](Files.readString(path.toJava)) match
          case Result.Success(manifest) => Result.Success(manifest)
          case Result.Failure(error)    =>
            Result.Failure(SquireError.Failure(
              "repo",
              "could not decode reference manifest",
              Present(error.getMessage)
            ))
      catch
        case error: java.io.IOException =>
          Result.Failure(SquireError.Failure("repo", "could not read reference manifest", Present(error.getMessage)))

  private def writeManifest(root: Path, manifest: ReferenceManifest): Result[SquireError, Unit] =
    val refs = root / ".refs"
    val path = refs / "manifest.json"
    try
      Files.createDirectories(refs.toJava)
      Files.writeString(path.toJava, SquireJson.encode(manifest) + "\n")
      Result.Success(())
    catch
      case error: java.io.IOException =>
        Result.Failure(SquireError.Failure("repo", "could not write reference manifest", Present(error.getMessage)))

  private def fromResultEffect[A](effect: Result[SquireError, A] < Sync): A < (Sync & Abort[SquireError]) =
    effect.flatMap {
      case Result.Success(value) => value
      case Result.Failure(error) => Abort.fail(error)
    }

  private def fromResult[A](result: Result[SquireError, A]): A < Abort[SquireError] =
    result match
      case Result.Success(value) => value
      case Result.Failure(error) => Abort.fail(error)

  private def failure(message: String): Result[SquireError, Unit] =
    Result.Failure(SquireError.Failure("repo", message))

  private def managedReferencePath(repo: ReferenceRepo, refs: Path): Result[SquireError, ManagedReferencePath] =
    def invalid(detail: String): Result[SquireError, ManagedReferencePath] =
      Result.Failure(SquireError.Failure("path", s"invalid reference path '${repo.path}'", Present(detail)))

    try
      val base      = refs.toJava.toAbsolutePath.normalize
      val candidate = base.resolve(repo.path).normalize
      if candidate == base || !candidate.startsWith(base) then invalid("path escapes its configured base")
      else if Files.isSymbolicLink(base) || !Files.isDirectory(base, LinkOption.NOFOLLOW_LINKS) then
        invalid("reference base is not an exact directory")
      else
        val baseReal                = base.toRealPath()
        val relative                = base.relativize(candidate)
        var current                 = base
        var index                   = 0
        var problem: Option[String] = None
        var missing                 = false
        while index < relative.getNameCount - 1 && problem.isEmpty && !missing do
          current = current.resolve(relative.getName(index))
          if Files.isSymbolicLink(current) then problem = Some("intermediate path component is a symbolic link")
          else if !Files.exists(current, LinkOption.NOFOLLOW_LINKS) then missing = true
          else if !Files.isDirectory(current, LinkOption.NOFOLLOW_LINKS) then
            problem = Some("intermediate path component is not a directory")
          else if !current.toRealPath().startsWith(baseReal) then
            problem = Some("intermediate directory resolves outside the reference base")
          index += 1

        problem match
          case Some(detail)    => invalid(detail)
          case None if missing =>
            Result.Success(ManagedReferencePath(Path(candidate.toString), ReferencePathKind.Missing))
          case None =>
            val parent = candidate.getParent
            if Files.isSymbolicLink(parent) || !Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS) then
              invalid("final parent is not an exact directory")
            else
              val parentReal = parent.toRealPath()
              if !parentReal.startsWith(baseReal) then invalid("final parent resolves outside the reference base")
              else if !Files.exists(candidate, LinkOption.NOFOLLOW_LINKS) then
                Result.Success(ManagedReferencePath(Path(candidate.toString), ReferencePathKind.Missing))
              else
                repo.strategy match
                  case "symlink" =>
                    if !Files.isSymbolicLink(candidate) then invalid("symlink strategy requires a final symbolic link")
                    else
                      val target =
                        try Some(Path(candidate.toRealPath().toString))
                        catch case _: java.nio.file.NoSuchFileException => None
                      Result.Success(ManagedReferencePath(Path(candidate.toString), ReferencePathKind.Symlink(target)))
                  case "clone" | "worktree" =>
                    if Files.isSymbolicLink(candidate) then
                      invalid(s"${repo.strategy} strategy forbids a final symbolic link")
                    else if !Files.isDirectory(candidate, LinkOption.NOFOLLOW_LINKS) then
                      invalid(s"${repo.strategy} strategy requires a directory")
                    else
                      val real     = candidate.toRealPath()
                      val expected = parentReal.resolve(candidate.getFileName).normalize
                      if !real.startsWith(baseReal) || real != expected then
                        invalid(s"${repo.strategy} directory is not the exact in-base destination")
                      else
                        Result.Success(
                          ManagedReferencePath(
                            Path(candidate.toString),
                            ReferencePathKind.Directory(Path(real.toString))
                          )
                        )
                  case other => invalid(s"unknown reference repository strategy: $other")
    catch
      case error: java.io.IOException => invalid(error.getMessage)
      case error: SecurityException   => invalid(error.getMessage)

  private def ownerFromSegments(parts: List[String]): Option[String] =
    parts.filter(_.nonEmpty) match
      case values if values.size >= 2 => Some(values(values.size - 2))
      case _                          => None

  private def relativePathFor(options: ReferenceAdd, name: String, org: Option[String]): String =
    if options.strategy == "worktree" then
      val repositoryName = sourceRepositoryName(options.urlOrPath)
      org match
        case Some(owner) => s"$owner/.worktrees/$repositoryName/$name"
        case None        => s".worktrees/$repositoryName/$name"
    else org.map(owner => s"$owner/$name").getOrElse(name)

  private def cloneFlags(options: ReferenceAdd): Chunk[String] =
    val depth = effectiveDepth(options) match
      case Some(value) => Chunk("--depth", value.toString)
      case None        => Chunk.empty
    val sparse = if options.sparse.nonEmpty then Chunk("--filter=blob:none", "--sparse") else Chunk.empty
    val ref    = options.ref match
      case Some(value) => Chunk("--branch", value, "--single-branch")
      case None        => Chunk.empty
    depth ++ sparse ++ ref

  private def effectiveDepth(options: ReferenceAdd): Option[Int] =
    if options.full then None else Some(options.depth.filter(_ != 0).getOrElse(1))

  private def isGithub(url: String): Boolean = url.contains("github.com")

  private def sourceRepositoryName(source: String): String =
    Option(java.nio.file.Path.of(source).toAbsolutePath.normalize.getFileName).fold("")(_.toString)

  private def validComponent(component: String): Boolean =
    component.nonEmpty && component != "." && component != ".." &&
      !component.contains('/') && !component.contains('\\') &&
      java.nio.file.Path.of(component).normalize.toString == component

  private def ensureDestinationAbsent(dest: Path): Unit < Abort[SquireError] =
    if Files.exists(dest.toJava, LinkOption.NOFOLLOW_LINKS) then
      Abort.fail(SquireError.Failure("repo", s"reference destination already exists: $dest"))
    else ()
