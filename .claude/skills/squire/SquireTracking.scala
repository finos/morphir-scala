//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala, SquireCellar.scala]

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, StandardOpenOption}
import kyo.*
import scala.jdk.CollectionConverters.*

enum TrackingMode derives Schema, CanEqual:
  case Auto, Beads, Off, Unavailable

final case class TrackingWorkspace(
    status: String,
    isWorktree: Boolean = false,
    localStore: Boolean = false,
    fallbackConfig: Maybe[String] = Absent,
    detail: Maybe[String] = Absent,
    remedy: Maybe[String] = Absent
) derives Schema

final case class GuidanceDrift(file: String, issue: String) derives Schema

final case class TrackingReport(
    configuredMode: TrackingMode,
    effectiveMode: TrackingMode,
    reason: String,
    bdInstalled: Boolean,
    bdVersion: Maybe[String],
    beadsDirPresent: Boolean,
    workspace: TrackingWorkspace,
    settingsFilePresent: Boolean,
    guidanceDrift: Chunk[GuidanceDrift],
    warning: Maybe[String] = Absent
) derives Schema

final case class GuidanceRewrite(text: String, removedBeadsBlocks: Int, changed: Boolean) derives CanEqual

final case class GuidanceSyncReport(
    output: String,
    exitCode: Int,
    changed: Chunk[String],
    missing: Chunk[String],
    unsafe: Chunk[String]
) derives CanEqual

trait TrackingFileSystem:
  def exists(path: Path): Boolean < Sync
  def read(path: Path): String < Sync
  def write(path: Path, text: String): Unit < Sync
  def children(path: Path): Chunk[Path] < Sync

object LiveTrackingFileSystem extends TrackingFileSystem:
  def exists(path: Path): Boolean < Sync = path.exists
  def read(path: Path): String < Sync = Sync.defer {
    val input = Files.newInputStream(path.toJava, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
    try new String(input.readAllBytes(), StandardCharsets.UTF_8)
    finally input.close()
  }
  def write(path: Path, text: String): Unit < Sync = Sync.defer {
    val output = Files.newOutputStream(
      path.toJava,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE,
      LinkOption.NOFOLLOW_LINKS
    )
    try output.write(text.getBytes(StandardCharsets.UTF_8))
    finally output.close()
  }
  def children(path: Path): Chunk[Path] < Sync = Sync.defer {
    if !Files.isDirectory(path.toJava) then Chunk.empty
    else
      val stream = Files.list(path.toJava)
      try Chunk.from(stream.iterator().asScala).map(child => Path(child.toString))
      finally stream.close()
  }

object SquireTracking:
  val settingsFile: String = ".config/squire/settings.local.yaml"
  val targets: Chunk[String] = Chunk("AGENTS.md", "CLAUDE.md")

  val pointer: String = """<!-- BEGIN MORPHIR TRACKING -->
## Task tracking

This project tracks multi-session work in [beads](https://github.com/steveyegge/beads) (`bd`).
**Using it is optional** — contributors can opt out, and nothing in the build, tests or CI reads
`.beads/`.

**Before tracking anything, resolve the mode** — do not assume beads is in play:

```bash
.claude/skills/squire/squire tracking status --quiet
```

- `beads` — use `bd` for work that outlives the session (anything with dependencies, or that
  needs to survive context compaction). Claim before starting; close with a reason.
- `off` — the contributor opted out. Do not run `bd` write commands; use session-scoped
  tracking and report follow-ups in your summary instead.
- `unavailable` — `bd` is not installed. Say so once, continue with session-scoped tracking,
  and don't install anything unprompted.

Session-scoped todo lists remain the right tool for the handful of steps you're about to take.
The test for beads is whether the context is still needed in two weeks.

**Full guidance, conventions and opt-out instructions: [docs/task-tracking.md](docs/task-tracking.md).**
That file governs over the more absolutist upstream bd guidance in
`.agents/skills/beads/SKILL.md` and `bd prime`, and a contributor's own instructions govern over
both. Neither `git push` nor `bd dolt push` is ever implied — both publish, and both need the
contributor's say-so.
<!-- END MORPHIR TRACKING -->"""

  enum GuidanceMode derives CanEqual:
    case Apply, Check, Diff

  private final case class YamlTracking(mode: Option[String] = None) derives Schema
  private final case class YamlSettings(tracking: YamlTracking = YamlTracking()) derives Schema

  def resolve(
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      files: TrackingFileSystem = LiveTrackingFileSystem
  ): TrackingReport < (Async & Sync & Abort[SquireError]) =
    for
      rawSettings <- loadSettings(root, files)
      (configured, invalidWarning) = configuredMode(rawSettings)
      version <- bdVersion(root, runner, platform)
      beadsPresent <- files.exists(root / ".beads")
      workspace <- resolveWorkspace(root, runner, files)
      guidance <- guidanceDrift(root, files)
    yield
      val installed = version.isDefined
      val (effective, reason) =
        if configured == TrackingMode.Off then (TrackingMode.Off, "contributor opted out via tracking.mode: off")
        else if !installed then (TrackingMode.Unavailable, "bd is not on PATH")
        else if !beadsPresent then (TrackingMode.Unavailable, ".beads does not exist in this checkout")
        else if workspace.status == "unresolvable" then
          (TrackingMode.Unavailable, workspace.detail.getOrElse("beads workspace cannot be resolved"))
        else if configured == TrackingMode.Beads then (TrackingMode.Beads, "tracking.mode: beads")
        else (TrackingMode.Beads, "bd is installed and .beads/ exists (mode: auto)")
      val warning =
        if configured == TrackingMode.Beads && effective == TrackingMode.Unavailable then
          Present(s"tracking.mode is 'beads' but $reason. Install bd, or set mode to auto/off. See docs/task-tracking.md.")
        else invalidWarning
      TrackingReport(
        configured,
        effective,
        reason,
        installed,
        version,
        beadsPresent,
        workspace,
        rawSettings.isDefined,
        guidance,
        warning
      )

  def rewriteGuidance(original: String): GuidanceRewrite =
    val (withoutBeads, removed) = removeBeadsBlocks(original)
    val withPointer = replaceOrAppendPointer(withoutBeads)
    val normalized = withoutTrailingNewlines(withPointer) + "\n"
    GuidanceRewrite(normalized, removed, normalized != original)

  def syncGuidance(
      root: Path,
      mode: GuidanceMode,
      files: TrackingFileSystem = LiveTrackingFileSystem
  ): GuidanceSyncReport < Sync =
    for
      state <- syncTargets(root, targets.toList, mode, files, Chunk.empty, Chunk.empty, Chunk.empty, "")
    yield
      val (changed, missing, unsafe, output) = state
      val exitCode = if missing.nonEmpty || unsafe.nonEmpty || ((mode == GuidanceMode.Check || mode == GuidanceMode.Diff) && changed.nonEmpty) then 1 else 0
      GuidanceSyncReport(output, exitCode, changed, missing, unsafe)

  private def syncTargets(
      root: Path,
      remaining: List[String],
      mode: GuidanceMode,
      files: TrackingFileSystem,
      changed: Chunk[String],
      missing: Chunk[String],
      unsafe: Chunk[String],
      output: String
  ): (Chunk[String], Chunk[String], Chunk[String], String) < Sync =
    remaining match
      case Nil => (changed, missing, unsafe, output)
      case name :: tail =>
        guidanceTarget(root, name) match
          case Result.Failure(error) =>
            syncTargets(root, tail, mode, files, changed, missing, unsafe.append(name), output + s"ISSUE - $name unsafe guidance target: ${error.getMessage}\n")
          case Result.Success(path) =>
            files.exists(path).flatMap {
              case false => syncTargets(root, tail, mode, files, changed, missing.append(name), unsafe, output + s"ISSUE - $name does not exist\n")
              case true =>
                files.read(path).flatMap { original =>
                  val rewrite = rewriteGuidance(original)
                  if !rewrite.changed then syncTargets(root, tail, mode, files, changed, missing, unsafe, output + s"OK - $name pointer is current\n")
                  else
                    val drift =
                      if rewrite.removedBeadsBlocks > 0 then s"DRIFT - $name carries ${rewrite.removedBeadsBlocks} bd-managed block(s); pointer needs reapplying\n"
                      else s"DRIFT - $name pointer is missing or stale\n"
                    mode match
                      case GuidanceMode.Apply =>
                        files.write(path, rewrite.text).flatMap(_ => syncTargets(root, tail, mode, files, changed.append(name), missing, unsafe, output + drift + s"  updated $name\n"))
                      case GuidanceMode.Check => syncTargets(root, tail, mode, files, changed.append(name), missing, unsafe, output + drift)
                      case GuidanceMode.Diff => syncTargets(root, tail, mode, files, changed.append(name), missing, unsafe, output + drift + unifiedDiff(name, original, rewrite.text))
                }
            }

  private def loadSettings(root: Path, files: TrackingFileSystem): Maybe[String] < Sync =
    val path = root / settingsFile
    files.exists(path).flatMap {
      case false => Absent
      case true => files.read(path).map(Present(_))
    }

  private def configuredMode(settings: Maybe[String]): (TrackingMode, Maybe[String]) =
    settings match
      case Absent => (TrackingMode.Auto, Absent)
      case Present(text) =>
        SquireYaml.decode[YamlSettings](text) match
          case Result.Success(value) => parseMode(value.tracking.mode)
          case Result.Failure(_) => (TrackingMode.Auto, Present(s"could not parse $settingsFile; using auto"))

  private def parseMode(raw: Option[String]): (TrackingMode, Maybe[String]) =
    raw.map(_.trim.toLowerCase) match
      case None | Some("") | Some("auto") => (TrackingMode.Auto, Absent)
      case Some("beads") | Some("true") => (TrackingMode.Beads, Absent)
      case Some("off") | Some("false") => (TrackingMode.Off, Absent)
      case Some(value) => (TrackingMode.Auto, Present(s"unrecognised tracking.mode '$value'; expected one of auto, beads, off"))

  private def bdVersion(root: Path, runner: ProcessRunner, platform: SquirePlatform): Maybe[String] < (Async & Abort[SquireError]) =
    platform.findExecutable("bd") match
      case Absent => Absent
      case Present(binary) =>
        runner.run(ProcessRequest(Chunk(binary, "version"), Present(root))).map { result =>
          if result.exitCode == 0 then result.stdout.linesIterator.concat(result.stderr.linesIterator).map(_.trim).find(_.nonEmpty) match
            case Some(value) => Present(value)
            case None        => Absent
          else Absent
        }

  private def resolveWorkspace(
      root: Path,
      runner: ProcessRunner,
      files: TrackingFileSystem
  ): TrackingWorkspace < (Async & Sync & Abort[SquireError]) =
    runner.run(ProcessRequest(Chunk("git", "rev-parse", "--git-dir", "--git-common-dir"), Present(root))).flatMap { result =>
      val paths = result.stdout.linesIterator.map(_.trim).filter(_.nonEmpty).toList
      if result.exitCode != 0 || paths.size < 2 then TrackingWorkspace("no-repo", detail = Present("not inside a git repository"))
      else
        val gitDir = gitPath(root, paths.head)
        val commonDir = gitPath(root, paths(1))
        val worktree = gitDir != commonDir
        val beads = root / ".beads"
        for
          localStore <- hasLocalStore(beads, files)
          result <-
            if !worktree || localStore then Sync.defer(TrackingWorkspace(if localStore then "local" else "shared", worktree, localStore))
            else
              val fallback =
                if commonDir.toJava.getFileName.toString == ".git" then Path(commonDir.toJava.getParent.resolve(".beads/config.yaml").toString)
                else commonDir / ".beads" / "config.yaml"
              files.exists(fallback).map {
                case true => TrackingWorkspace("shared", worktree, localStore, Present(fallback.toString))
                case false =>
                  val detail = s"this is a git worktree with no beads workspace of its own, and the main clone's $fallback does not exist"
                  TrackingWorkspace("unresolvable", worktree, localStore, Present(fallback.toString), Present(detail), Present("run `bd bootstrap` here to clone the workspace from the remote ref"))
              }
        yield result
    }

  private def hasLocalStore(beads: Path, files: TrackingFileSystem): Boolean < Sync =
    def directoryExists(names: List[String]): Boolean < Sync =
      names match
        case Nil => false
        case name :: tail => files.exists(beads / name).flatMap(if _ then true else directoryExists(tail))
    directoryExists(List("embeddeddolt", "dolt", "proxieddb")).flatMap { directoryFound =>
      if directoryFound then true
      else files.children(beads).map(_.exists(_.toJava.getFileName.toString.endsWith(".db")))
    }

  private def guidanceDrift(root: Path, files: TrackingFileSystem): Chunk[GuidanceDrift] < Sync =
    def loop(remaining: List[String], drift: Chunk[GuidanceDrift]): Chunk[GuidanceDrift] < Sync =
      remaining match
        case Nil => drift
        case name :: tail =>
          guidanceTarget(root, name) match
            case Result.Failure(error) => loop(tail, drift.append(GuidanceDrift(name, s"unsafe guidance target: ${error.getMessage}")))
            case Result.Success(path) => files.exists(path).flatMap {
              case false => loop(tail, drift)
              case true => files.read(path).flatMap { text =>
                val issue =
                  if text.contains("BEGIN BEADS INTEGRATION") || text.contains("BEGIN BEADS CODEX SETUP") then
                    Present("bd-managed guidance block present; expected the repo pointer")
                  else if !text.contains("BEGIN MORPHIR TRACKING") then Present("no tracking pointer block found")
                  else Absent
                loop(tail, issue.map(message => drift.append(GuidanceDrift(name, message))).getOrElse(drift))
              }
            }
    loop(targets.toList, Chunk.empty)

  private def guidanceTarget(root: Path, name: String): Result[SquireError, Path] =
    try
      val base = root.toJava.toAbsolutePath.normalize
      val candidate = base.resolve(name).normalize
      if !Files.exists(base, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(base) ||
        !Files.isDirectory(base, LinkOption.NOFOLLOW_LINKS)
      then Result.Failure(SquireError.Failure("tracking", "repository root is not an exact directory"))
      else if !candidate.startsWith(base) || candidate == base then
        Result.Failure(SquireError.Failure("tracking", "guidance target escapes repository root"))
      else if Files.exists(candidate, LinkOption.NOFOLLOW_LINKS) && Files.isSymbolicLink(candidate) then
        Result.Failure(SquireError.Failure("tracking", "guidance target is a symbolic link"))
      else SquirePaths.resolveUnder(Path(candidate.toString), Path(base.toString))
    catch
      case error: java.io.IOException => Result.Failure(SquireError.Failure("tracking", "could not resolve guidance target", Present(error.getMessage)))

  private def gitPath(root: Path, rendered: String): Path =
    val path = java.nio.file.Path.of(rendered)
    if path.isAbsolute then Path(path.normalize.toString) else root / rendered

  private def removeBeadsBlocks(text: String): (String, Int) =
    List("BEADS INTEGRATION", "BEADS CODEX SETUP").foldLeft((text, 0)) { case ((current, count), marker) =>
      val regex = s"(?s)\\n*<!-- BEGIN $marker.*?<!-- END $marker -->\\n*".r
      val matches = regex.findAllMatchIn(current).length
      (regex.replaceAllIn(current, "\n\n"), count + matches)
    }

  private def replaceOrAppendPointer(text: String): String =
    val begin = "<!-- BEGIN MORPHIR TRACKING -->"
    val end = "<!-- END MORPHIR TRACKING -->"
    if text.contains(begin) && text.contains(end) then
      s"(?s)${java.util.regex.Pattern.quote(begin)}.*?${java.util.regex.Pattern.quote(end)}".r.replaceAllIn(text, pointer)
    else withoutTrailingNewlines(text) + "\n\n" + pointer

  private def withoutTrailingNewlines(text: String): String = text.replaceFirst("[\\r\\n]+$", "")

  private def unifiedDiff(name: String, original: String, updated: String): String =
    s"--- a/$name\n+++ b/$name\n@@ -1,${original.linesIterator.size} +1,${updated.linesIterator.size} @@\n" +
      original.linesIterator.map(line => s"-$line\n").mkString + updated.linesIterator.map(line => s"+$line\n").mkString
