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
    val name = options.name.filter(_.nonEmpty).getOrElse(nameFrom(options.urlOrPath))
    if manifest.repos.exists(_.name == name) then failure(s"repo '$name' already in manifest")
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
        status(root, None, runner).map { report =>
          val header = f"${"NAME"}%-20s ${"PATH"}%-28s ${"STRATEGY"}%-10s ${"REF"}%-20s STATUS\n" + ("-" * 100) + "\n"
          val rows   = manifest.repos.map { repo =>
            val marker = if repo.sparse.nonEmpty then " [sparse]" else ""
            val state  =
              if report.output.contains(s"[${repo.name}]\n") then "see status" + marker else "MISSING" + marker
            f"${repo.name}%-20s ${repo.path}%-28s ${repo.strategy}%-10s ${repo.ref.getOrElse("").take(20)}%-20s $state"
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
            val output = entries.map((repo, detail, _) => s"\n[${repo.name}]\n$detail\n").mkString
            ReferenceStatusReport(output, if entries.forall(_._3) then 0 else 1)
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
          val dest = refs / entry.path
          if keepFiles then saveManifest(root, ReferenceManifest(manifest.repos.filterNot(_.name == name)))
          else
            fromResult(lexicalPathUnder(dest, refs)).flatMap { safeLexicalDest =>
              removeFiles(entry, safeLexicalDest, refs, runner).flatMap { _ =>
                pruneEmptyParents(safeLexicalDest.parent.get, refs).flatMap { _ =>
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
  ): List[(ReferenceRepo, String, Boolean)] < (Async & Sync & Abort[SquireError]) =
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
  ): (ReferenceRepo, String, Boolean) < (Async & Sync & Abort[SquireError]) =
    val path   = root / ".refs" / repo.path
    val prefix = List(
      s"  name:     ${repo.name}",
      s"  path:     $path",
      s"  strategy: ${repo.strategy}"
    ) ++ repo.url.map(value => s"  url:      $value") ++
      repo.source.map(value => s"  source:   $value") ++
      (if repo.sparse.nonEmpty then List(s"  sparse:   ${repo.sparse.mkString(" ")}") else Nil) ++
      List(
        s"  ref:      ${repo.ref.getOrElse("?")}",
        s"  recorded: ${repo.commit.getOrElse("?")}",
        s"  added:    ${repo.added}"
      )
    if Files.isSymbolicLink(path.toJava) && !Files.exists(path.toJava) then
      val detail = (prefix :+ "  disk:     broken symlink" :+ "  BROKEN_SYMLINK — target does not exist").mkString("\n")
      (repo, detail, false)
    else if !Files.exists(path.toJava) then
      (repo, (prefix :+ "  MISSING — not found on disk").mkString("\n"), false)
    else
      for
        current     <- gitValue(path, runner, Chunk("rev-parse", "HEAD"))
        branch      <- gitValue(path, runner, Chunk("symbolic-ref", "--short", "HEAD"))
        dirtyResult <- runner.run(ProcessRequest(Chunk("git", "-C", path.toString, "status", "--porcelain")))
      yield
        val dirty = dirtyResult.exitCode == 0 && dirtyResult.stdout.trim.nonEmpty
        val drift = current.nonEmpty && current != repo.commit
        val state =
          List(s"  current:  ${current.getOrElse("?")} (${branch.getOrElse("(detached)")})") ++
            (if drift then List("  DRIFT — manifest commit differs from current HEAD") else Nil) ++
            (if dirty then List("  DIRTY — uncommitted changes present") else Nil) ++
            (if !drift && !dirty then List("  in sync with manifest") else Nil)
        (repo, (prefix ++ state).mkString("\n"), !drift && !dirty)

  private def removeFiles(
      entry: ReferenceRepo,
      dest: Path,
      refs: Path,
      runner: ProcessRunner
  ): Unit < (Async & Sync & Abort[SquireError]) =
    entry.strategy match
      case "symlink" =>
        Sync.defer {
          if Files.isSymbolicLink(dest.toJava) then Files.delete(dest.toJava)
        }
      case "worktree" =>
        fromResult(SquirePaths.resolveUnder(dest, refs)).flatMap { safeDest =>
          entry.source match
            case Some(source) if Files.exists(java.nio.file.Path.of(source)) =>
              runChecked(
                runner,
                ProcessRequest(Chunk("git", "-C", source, "worktree", "remove", "--force", safeDest.toString))
              ).unit
            case _ => deleteTree(safeDest)
        }
      case _ =>
        if Files.exists(dest.toJava, LinkOption.NOFOLLOW_LINKS) then
          fromResult(SquirePaths.resolveUnder(dest, refs)).flatMap(deleteTree)
        else ()

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

  private def lexicalPathUnder(candidate: Path, base: Path): Result[SquireError, Path] =
    val basePath      = base.toJava.toAbsolutePath.normalize
    val candidatePath = candidate.toJava.toAbsolutePath.normalize
    if candidatePath != basePath && candidatePath.startsWith(basePath) then Result.Success(Path(candidatePath.toString))
    else Result.Failure(SquireError.Failure("path", "path escapes its configured base"))

  private def ownerFromSegments(parts: List[String]): Option[String] =
    parts.filter(_.nonEmpty) match
      case values if values.size >= 2 => Some(values(values.size - 2))
      case _                          => None

  private def relativePathFor(options: ReferenceAdd, name: String, org: Option[String]): String =
    if options.strategy == "worktree" then
      val repositoryName = java.nio.file.Path.of(options.urlOrPath).toAbsolutePath.normalize.getFileName.toString
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
    if options.full then None else Some(options.depth.getOrElse(1))

  private def isGithub(url: String): Boolean = url.contains("github.com")
