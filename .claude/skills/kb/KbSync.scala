//| scalaVersion: 3.8.4
//| moduleDeps: [KbScaffold.scala, KbCheck.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5
//| - org.yaml:snakeyaml:2.6

/** Vendoring external documents into a bundle, and projecting them back out.
  *
  * A bundle may declare a `sync.yaml` naming an upstream repository and the paths it mirrors. Markdown lands as OKF
  * concepts; everything else lands as byte-identical assets. The knowledge base owns a fenced region inside each
  * mirrored concept's frontmatter, and [[project]] removes exactly that region — so the file that goes back upstream
  * is the file that came from it.
  *
  * The whole design rests on one invariant, pinned by `KbSyncSpec`:
  *
  * {{{ project(inject(bytes)) == bytes }}}
  *
  * byte for byte, including line endings. That is why nothing here re-serializes YAML: upstream frontmatter is moved
  * around as lines, never parsed and rewritten, so a fractional `sidebar_position` or a nested `tracking:` block
  * cannot be reformatted by accident.
  */

import kyo.*
import scala.jdk.CollectionConverters.*
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
import org.yaml.snakeyaml.constructor.SafeConstructor
import java.time.LocalDate

// --------------------------------------------------------------------- model

/** One `from:` entry in the manifest, with the globs that carve exceptions out of it. */
case class SyncMapping(from: String, exclude: Seq[String])

/** A bundle's `sync.yaml` — hand-written, reviewed, and the only place upstream coordinates live. */
case class SyncManifest(
    repo: String,
    ref: String,
    refsPath: String,
    root: String,
    mappings: Seq[SyncMapping],
    exclude: Seq[String],
    /** Ordered: the first glob that matches supplies the concept's `type`. */
    typeMap: Seq[(String, String)]
):
  /** Concepts are markdown; everything else rides along untouched. `.mdx` is deliberately an asset — commonmark has
    * no business parsing JSX, and widening the markdown predicate would reclassify files in every other bundle.
    */
  def kindOf(path: String): SyncKind =
    if path.endsWith(".md") then SyncKind.Concept else SyncKind.Asset

  def typeFor(path: String): String =
    typeMap.collectFirst { case (glob, t) if KbGlob.matches(glob, path) => t }.getOrElse("Source Document")

  def selects(path: String): Boolean =
    val excluded = exclude.exists(KbGlob.matches(_, path))
    !excluded && mappings.exists { m =>
      KbGlob.matches(m.from, path) && !m.exclude.exists(KbGlob.matches(_, path))
    }

enum SyncKind:
  case Concept, Asset
  def label: String = this match
    case Concept => "concept"
    case Asset => "asset"

object SyncKind:
  def parse(s: String): SyncKind = if s == "asset" then Asset else Concept

/** One mirrored file as of the last import. `upstreamSha256` hashes the *upstream* form, so a local file is compared
  * by projecting it first — there is no second hash to keep honest.
  */
case class LockEntry(path: String, kind: SyncKind, upstreamSha256: String)

case class SyncLock(baseCommit: String, importedAt: String, files: Seq[LockEntry]):
  private lazy val byPath: Map[String, LockEntry] = files.map(e => e.path -> e).toMap
  def get(path: String): Option[LockEntry] = byPath.get(path)

object SyncLock:
  val Empty: SyncLock = SyncLock("", "", Nil)

/** Where a mirrored file stands relative to the import baseline. Derived on every run, never stored. */
enum SyncState:
  case Clean, LocalOnly, UpstreamOnly, Diverged, MissingLocal, DeletedUpstream, Untracked, Unreadable

  def label: String = this match
    case Clean => "clean"
    case LocalOnly => "local-only"
    case UpstreamOnly => "upstream-only"
    case Diverged => "diverged"
    case MissingLocal => "missing-local"
    case DeletedUpstream => "deleted-upstream"
    case Untracked => "untracked"
    case Unreadable => "unreadable"

case class FileStatus(path: String, kind: SyncKind, state: SyncState, detail: String = "")

/** What a `pull` or `push` did, or would do under `--dry-run`. Mirrors `RefreshAction` so both render the same way. */
case class SyncAction(verb: String, path: String, detail: String)

/** A bundle together with the manifest and lockfile that make it a mirror. */
case class SyncBundle(bundle: Bundle, manifest: SyncManifest, lock: SyncLock):
  def mirrorRoot: Path = KbSync.resolve(bundle.root, manifest.root)
  def localFile(rel: String): Path = KbSync.resolve(mirrorRoot, rel)

// ---------------------------------------------------------------------- glob

/** Glob matching over `/`-separated relative paths. Supports `*`, `?` and `**`. */
object KbGlob:
  private val cache = collection.concurrent.TrieMap.empty[String, scala.util.matching.Regex]

  def matches(glob: String, path: String): Boolean =
    cache.getOrElseUpdate(glob, compile(glob)).matches(path)

  private def compile(glob: String): scala.util.matching.Regex =
    val sb = StringBuilder("^")
    var i = 0
    while i < glob.length do
      val c = glob.charAt(i)
      if c == '*' && i + 1 < glob.length && glob.charAt(i + 1) == '*' then
        // `**/` also matches zero directories, so `docs/**/x.md` finds `docs/x.md`.
        if i + 2 < glob.length && glob.charAt(i + 2) == '/' then
          sb ++= "(?:.*/)?"
          i += 3
        else
          sb ++= ".*"
          i += 2
      else
        i += 1
        c match
          case '*' => sb ++= "[^/]*"
          case '?' => sb ++= "[^/]"
          case ch if "\\.+()^$|{}[]".contains(ch) => sb ++= "\\" + ch
          case ch => sb += ch
    sb ++= "$"
    sb.toString.r

// ---------------------------------------------------------------- projection

object KbSync:

  val FenceBegin = "# kb:begin"
  val FenceEnd = "# kb:end"
  private val FenceNote = " — added by the knowledge base; removed on export"

  /** Written into the opening fence when the frontmatter block itself is ours.
    *
    * Without it, a document whose upstream frontmatter is an *empty* `---` / `---` pair is indistinguishable from one
    * that had no frontmatter at all, and export would delete a block upstream actually has.
    */
  private val WholeBlockFlag = "block"

  /** The generated region in a sync bundle's index. */
  val IndexMarker = "<!-- kb:sources -->"

  val ManifestName = "sync.yaml"
  val LockName = "sync.lock.yaml"

  /** A document split at its frontmatter fences, preserving every byte: `open + fm + close + body == text`. */
  case class Split(open: String, fm: String, close: String, body: String)

  /** Splits without normalizing line endings.
    *
    * `KbStore.splitFrontmatter` converts CRLF to LF, which is right for parsing and fatal for round-tripping — a CRLF
    * file would come back from `project` with different bytes than it went in with.
    */
  def split(text: String): Option[Split] =
    val open =
      if text.startsWith("---\r\n") then Some("---\r\n")
      else if text.startsWith("---\n") then Some("---\n")
      else None
    open.flatMap { o =>
      val rest = text.drop(o.length)
      fenceAt(rest).map { (start, len) =>
        Split(o, rest.take(start), rest.substring(start, start + len), rest.drop(start + len))
      }
    }

  /** Offset and length of the first line that is exactly `---`, including its line terminator. */
  private def fenceAt(s: String): Option[(Int, Int)] =
    var idx = 0
    var found: Option[(Int, Int)] = None
    while found.isEmpty && idx <= s.length do
      val nl = s.indexOf('\n', idx)
      val lineEnd = if nl == -1 then s.length else nl
      val line = s.substring(idx, lineEnd)
      if line.trim == "---" then found = Some((idx, (if nl == -1 then lineEnd else nl + 1) - idx))
      else if nl == -1 then idx = s.length + 1
      else idx = nl + 1
    found

  private def eolOf(text: String): String = if text.contains("\r\n") then "\r\n" else "\n"

  /** Splits into lines, each keeping its terminator, so `mkString` rebuilds the input exactly. */
  private def linesKeepingEol(s: String): Vector[String] =
    val out = Vector.newBuilder[String]
    var idx = 0
    while idx < s.length do
      val nl = s.indexOf('\n', idx)
      val end = if nl == -1 then s.length else nl + 1
      out += s.substring(idx, end)
      idx = end
    out.result()

  /** Adds the knowledge base's own frontmatter keys inside a fenced region.
    *
    * When the document has no frontmatter at all, the whole block is ours and `project` removes all of it.
    */
  def inject(text: String, keys: Seq[(String, String)]): String =
    val eol = eolOf(text)
    def block(whole: Boolean): String =
      val open = if whole then s"$FenceBegin $WholeBlockFlag$FenceNote" else FenceBegin + FenceNote
      (Seq(open) ++ keys.map((k, v) => s"$k: $v") ++ Seq(FenceEnd)).map(_ + eol).mkString
    split(text) match
      case Some(s) => s.open + s.fm + block(false) + s.close + s.body
      case None => s"---$eol" + block(true) + s"---$eol" + text

  /** Removes the fenced region, yielding the upstream form. Left when the fence is damaged. */
  def project(text: String): Either[String, String] =
    split(text) match
      case None => Right(text)
      case Some(s) =>
        val lines = linesKeepingEol(s.fm)
        val b = lines.indexWhere(_.trim.startsWith(FenceBegin))
        val e = lines.indexWhere(_.trim == FenceEnd)
        if b < 0 && e < 0 then Right(text)
        else if b < 0 then Left(s"$FenceEnd without $FenceBegin")
        else if e < 0 then Left(s"$FenceBegin without $FenceEnd")
        else if e < b then Left("kb fence closes before it opens")
        else
          val kept = lines.take(b) ++ lines.drop(e + 1)
          val wholeBlock = lines(b).trim.stripPrefix(FenceBegin).trim.startsWith(WholeBlockFlag)
          // The block goes only when we created it *and* nobody has since added a key of their own to it.
          if wholeBlock && kept.isEmpty then Right(s.body)
          else Right(s.open + kept.mkString + s.close + s.body)

  /** The keys the knowledge base injects: `type` always, plus whatever OKF needs and upstream did not supply. */
  def injectedKeys(manifest: SyncManifest, path: String, upstream: String): Seq[(String, String)] =
    val fm = split(upstream).flatMap(s => KbStore.parseFrontmatter(s.fm).toOption).getOrElse(Frontmatter.Empty)
    val name = path.split("/").lastOption.getOrElse(path).stripSuffix(".md")
    val fallbackTitle = name.split("[-_]").filter(_.nonEmpty).map(_.capitalize).mkString(" ")
    Seq("type" -> manifest.typeFor(path)) ++
      Option.when(fm.title.isEmpty)("title" -> yamlStr(fallbackTitle)) ++
      Option.when(fm.description.isEmpty)(
        "description" -> yamlStr(s"Upstream source document ${manifest.repo}:$path.")
      ) ++
      Seq("kb_upstream" -> path)

  private def yamlStr(s: String): String =
    val needsQuote = s.exists(c => ":#{}[]&*!|>'\"%@`,".contains(c)) || s.startsWith(" ") || s.endsWith(" ")
    if needsQuote then "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"" else s

  // ------------------------------------------------------------------- yaml

  private def newYaml(): Yaml =
    val opts = LoaderOptions()
    opts.setAllowDuplicateKeys(false)
    Yaml(SafeConstructor(opts))

  private def asMap(o: Any): Map[String, Any] = o match
    case m: java.util.Map[?, ?] => m.asScala.toMap.map((k, v) => k.toString -> v.asInstanceOf[Any])
    case _ => Map.empty

  private def asList(o: Any): List[Any] = o match
    case l: java.util.List[?] => l.asScala.toList.map(_.asInstanceOf[Any])
    case _ => Nil

  private def str(m: Map[String, Any], k: String): Option[String] = m.get(k).collect {
    case s: String => s
    case i: Integer => i.toString
  }

  private def strs(m: Map[String, Any], k: String): Seq[String] = m.get(k) match
    case Some(l: java.util.List[?]) => l.asScala.toList.collect { case s: String => s }
    case Some(s: String) => Seq(s)
    case _ => Nil

  def parseManifest(raw: String): Either[String, SyncManifest] =
    try
      val top = asMap(newYaml().load[Object](raw))
      val up = asMap(top.getOrElse("upstream", null))
      str(up, "repo") match
        case None => Left("sync.yaml needs `upstream.repo`, e.g. `finos/morphir`")
        case Some(repo) =>
          val mappings = asList(top.getOrElse("mappings", null)).flatMap { entry =>
            entry match
              case s: String => Some(SyncMapping(s, Nil))
              case other =>
                val m = asMap(other)
                str(m, "from").map(f => SyncMapping(f, strs(m, "exclude")))
          }
          if mappings.isEmpty then Left("sync.yaml needs at least one entry under `mappings:`")
          else
            Right(SyncManifest(
              repo = repo,
              ref = str(up, "ref").getOrElse("main"),
              refsPath = str(up, "refs_path").getOrElse(repo),
              root = str(top, "root").getOrElse("sources"),
              mappings = mappings,
              exclude = strs(top, "exclude"),
              // SnakeYAML preserves mapping order, and order decides which glob wins.
              typeMap = asMap(top.getOrElse("type_map", null)).toSeq.collect { case (k, v: String) => k -> v }
            ))
    catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString).linesIterator.next())

  def parseLock(raw: String): Either[String, SyncLock] =
    try
      val top = asMap(newYaml().load[Object](raw))
      val files = asList(top.getOrElse("files", null)).flatMap { entry =>
        val m = asMap(entry)
        for
          p <- str(m, "path")
          h <- str(m, "upstream_sha256")
        yield LockEntry(p, SyncKind.parse(str(m, "kind").getOrElse("concept")), h)
      }
      Right(SyncLock(str(top, "base_commit").getOrElse(""), str(top, "imported_at").getOrElse(""), files))
    catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString).linesIterator.next())

  def renderLock(lock: SyncLock): String =
    val sb = StringBuilder()
    sb ++= "# Generated by `kb sync pull`. Do not edit by hand.\n"
    sb ++= s"base_commit: ${lock.baseCommit}\n"
    sb ++= s"imported_at: ${lock.importedAt}\n"
    sb ++= "files:\n"
    lock.files.sortBy(_.path).foreach { e =>
      sb ++= s"  - { path: ${e.path}, kind: ${e.kind.label}, upstream_sha256: ${e.upstreamSha256} }\n"
    }
    sb.toString

  // --------------------------------------------------------------------- io

  def sha256(bytes: Array[Byte]): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(b => String.format("%02x", Integer.valueOf(b & 0xff))).mkString

  private def jpath(p: Path): java.nio.file.Path = java.nio.file.Path.of(KbPath.render(p))

  def readBytes(p: Path): Array[Byte] < (Sync & Abort[Throwable]) =
    Sync.defer(java.nio.file.Files.readAllBytes(jpath(p)))

  def writeBytes(p: Path, bytes: Array[Byte]): Unit < (Sync & Abort[Throwable]) =
    Sync.defer {
      val target = jpath(p)
      Option(target.getParent).foreach(java.nio.file.Files.createDirectories(_))
      java.nio.file.Files.write(target, bytes)
      ()
    }

  def deleteFile(p: Path): Unit < (Sync & Abort[Throwable]) =
    Sync.defer { java.nio.file.Files.deleteIfExists(jpath(p)); () }

  /** Every file at or below `dir`, as `/`-separated paths relative to it. */
  def relativeFilesUnder(dir: Path): Chunk[String] < (Sync & Abort[Throwable]) =
    def walk(d: Path, prefix: String): Chunk[String] < (Sync & Abort[Throwable]) =
      d.list.map { entries =>
        Kyo.foreach(entries) { e =>
          val name = e.parts.lastOption.getOrElse("")
          val rel = if prefix.isEmpty then name else s"$prefix/$name"
          e.isDirectory.map {
            // `.git` is never content, and walking it on a full checkout costs seconds.
            case true => if name == ".git" then Chunk.empty[String] else walk(e, rel)
            case false => Chunk(rel)
          }
        }.map(_.flattenChunk)
      }
    dir.exists.map {
      case false => Chunk.empty[String]
      case true => walk(dir, "")
    }

  def resolve(root: Path, rel: String): Path =
    rel.split("/").filter(_.nonEmpty).foldLeft(root)(_ / _)

  /** Rejects a manifest path that would write outside the mirror — the same guard `add-concept` carries. */
  def safeRelative(rel: String): Boolean =
    rel.nonEmpty && !rel.startsWith("/") && !rel.split("/").exists(s => s == ".." || s == ".")

  // ----------------------------------------------------------------- loading

  /** The bundle declaring `sync: true`, or a named one. Nothing is hardcoded, so this works in any repository. */
  def findBundle(kb: Kb, label: Option[String]): Option[Bundle] =
    label match
      case Some(l) => kb.bundle(l)
      // Truthiness matters: `sync: false` is a bundle saying no, and treating mere presence as yes would make
      // turning the marker off require deleting it.
      case None => kb.bundles.find(_.index.fm.str("sync").exists(v => v == "true" || v == "yes"))

  def load(b: Bundle): Either[String, SyncBundle] < (Sync & Abort[Throwable]) =
    val mf = b.root / ManifestName
    val lf = b.root / LockName
    mf.exists.map {
      case false => Left(s"${b.label} has no $ManifestName")
      case true =>
        for
          rawM <- mf.read
          hasLock <- lf.exists
          rawL <- if hasLock then lf.read else ("": String < (Sync & Abort[Throwable]))
        yield parseManifest(rawM).flatMap { m =>
          val lock = if hasLock then parseLock(rawL) else Right(SyncLock.Empty)
          lock.map(SyncBundle(b, m, _))
        }
    }

  // ------------------------------------------------------------------ status

  /** Compares local, baseline and upstream for one path. Upstream is optional so status works without a checkout. */
  private def stateOf(
      lockHash: Option[String],
      localUpstreamForm: Option[Either[String, String]],
      upstreamHash: Option[String]
  ): SyncState =
    (lockHash, localUpstreamForm, upstreamHash) match
      case (_, Some(Left(_)), _) => SyncState.Unreadable
      case (None, _, _) => SyncState.Untracked
      case (Some(_), None, _) => SyncState.MissingLocal
      case (Some(base), Some(Right(local)), up) =>
        val localHash = sha256(local.getBytes("UTF-8"))
        val localChanged = localHash != base
        up match
          case None => if localChanged then SyncState.LocalOnly else SyncState.Clean
          // Agreement beats the baseline. After an export the checkout holds our change, so both sides differ from
          // the recorded hash while being identical to each other — there is nothing to send and nothing to take.
          // Without this, pushing twice into the same checkout reports divergence the first push created.
          case Some(h) if h == localHash => SyncState.Clean
          case Some(h) if h == base => if localChanged then SyncState.LocalOnly else SyncState.Clean
          case Some(_) => if localChanged then SyncState.Diverged else SyncState.UpstreamOnly

  /** Reads a mirrored file and reduces it to its upstream form. Assets are already in it. */
  private def upstreamFormOf(sb: SyncBundle, rel: String, kind: SyncKind): Option[Either[String, String]] < (Sync & Abort[Throwable]) =
    val f = sb.localFile(rel)
    f.exists.map {
      case false => (None: Option[Either[String, String]])
      case true =>
        readBytes(f).map { bytes =>
          val text = String(bytes, "UTF-8")
          Some(kind match
            case SyncKind.Concept => project(text)
            case SyncKind.Asset => Right(text))
        }
    }

  def status(sb: SyncBundle, upstreamRoot: Option[Path]): Seq[FileStatus] < (Sync & Abort[Throwable]) =
    for
      upstreamRels <- upstreamRoot match
        case None => (Chunk.empty[String]: Chunk[String] < (Sync & Abort[Throwable]))
        case Some(r) => relativeFilesUnder(r).map(_.filter(sb.manifest.selects))
      upstreamHashes <- Kyo.foreach(upstreamRels) { rel =>
        readBytes(resolve(upstreamRoot.get, rel)).map(b => rel -> sha256(b))
      }.map(_.toMap)
      paths = (sb.lock.files.map(_.path) ++ upstreamRels).distinct.sorted
      rows <- Kyo.foreach(Chunk.from(paths)) { rel =>
        val entry = sb.lock.get(rel)
        val kind = entry.map(_.kind).getOrElse(sb.manifest.kindOf(rel))
        upstreamFormOf(sb, rel, kind).map { local =>
          val up = upstreamHashes.get(rel)
          val state =
            if entry.isDefined && upstreamRoot.isDefined && up.isEmpty then SyncState.DeletedUpstream
            else stateOf(entry.map(_.upstreamSha256), local, up)
          val detail = local match
            case Some(Left(err)) => err
            case _ => ""
          FileStatus(rel, kind, state, detail)
        }
      }
    yield rows.toSeq

  // -------------------------------------------------------------------- pull

  case class PullResult(actions: Seq[SyncAction], lock: SyncLock, refused: Seq[FileStatus])

  def pull(
      sb: SyncBundle,
      upstreamRoot: Path,
      baseCommit: String,
      today: LocalDate,
      dryRun: Boolean,
      theirs: Boolean,
      prune: Boolean
  ): PullResult < (Sync & Abort[Throwable]) =
    for
      rows <- status(sb, Some(upstreamRoot))
      results <- Kyo.foreach(Chunk.from(rows)) { st =>
        val rel = st.path
        val importIt = st.state match
          case SyncState.Untracked | SyncState.UpstreamOnly | SyncState.MissingLocal => true
          case SyncState.Diverged | SyncState.Unreadable => theirs
          case _ => false
        if !safeRelative(rel) then
          (Chunk((Some(SyncAction("refused", rel, "path escapes the mirror")), None, Some(st)))
            : Chunk[(Option[SyncAction], Option[LockEntry], Option[FileStatus])] < (Sync & Abort[Throwable]))
        else if st.state == SyncState.DeletedUpstream then
          val act = if prune then "removed" else "gone upstream"
          val doIt = prune && !dryRun
          (if doIt then deleteFile(sb.localFile(rel)) else ((): Unit < (Sync & Abort[Throwable])))
            .andThen(Chunk((
              Some(SyncAction(act, rel, "no longer present upstream")),
              Option.when(!prune)(sb.lock.get(rel)).flatten,
              None
            )))
        else if !importIt then
          val refused = Option.when(st.state == SyncState.Diverged || st.state == SyncState.Unreadable)(st)
          if refused.isDefined then
            (Chunk((None, sb.lock.get(rel), refused))
              : Chunk[(Option[SyncAction], Option[LockEntry], Option[FileStatus])] < (Sync & Abort[Throwable]))
          else
            // A clean file whose recorded hash is stale — both sides moved to the same content, which is what an
            // export leaves behind — gets its baseline refreshed. No write, just the lock catching up.
            readBytes(resolve(upstreamRoot, rel)).map { bytes =>
              val hash = sha256(bytes)
              val entry = sb.lock.get(rel).map(e => e.copy(upstreamSha256 = hash))
              val rebaselined = sb.lock.get(rel).exists(_.upstreamSha256 != hash)
              Chunk((Option.when(rebaselined)(SyncAction("rebaselined", rel, "already in step upstream")), entry, None))
            }
        else
          val src = resolve(upstreamRoot, rel)
          readBytes(src).map { bytes =>
            val hash = sha256(bytes)
            val kind = sb.manifest.kindOf(rel)
            val text = String(bytes, "UTF-8")
            val out = kind match
              case SyncKind.Concept => inject(text, injectedKeys(sb.manifest, rel, text)).getBytes("UTF-8")
              case SyncKind.Asset => bytes
            val verb = if sb.lock.get(rel).isEmpty then "added" else "updated"
            (if dryRun then ((): Unit < (Sync & Abort[Throwable])) else writeBytes(sb.localFile(rel), out))
              .andThen(Chunk((Some(SyncAction(verb, rel, kind.label)), Some(LockEntry(rel, kind, hash)), None)))
          }
      }.map(_.flattenChunk)
    yield
      val actions = results.flatMap(_._1).toSeq
      val entries = results.flatMap(_._2).toSeq
      val refused = results.flatMap(_._3).toSeq
      PullResult(actions, SyncLock(baseCommit, today.toString, entries), refused)

  def writeLock(sb: SyncBundle, lock: SyncLock): Unit < (Sync & Abort[Throwable]) =
    (sb.bundle.root / LockName).write(renderLock(lock))

  // -------------------------------------------------------------------- push

  /** Writes the upstream form of everything changed here into `target`.
    *
    * `upstreamRoot` is what makes `includeDiverged` mean anything: without it a file that moved on both sides is
    * indistinguishable from one that only moved here, and would be exported silently — overwriting upstream's own
    * change. Pass the checkout whenever there is one.
    */
  def push(sb: SyncBundle, target: Path, upstreamRoot: Option[Path], dryRun: Boolean, includeDiverged: Boolean)
      : (Seq[SyncAction], Seq[FileStatus]) < (Sync & Abort[Throwable]) =
    for
      rows <- status(sb, upstreamRoot)
      results <- Kyo.foreach(Chunk.from(rows)) { st =>
        val exportable = st.state match
          case SyncState.LocalOnly => true
          case SyncState.Diverged => includeDiverged
          case _ => false
        // A diverged file held back is reported rather than passed over in silence: it is the one case where doing
        // nothing loses work, because somebody has to reconcile the two changes by hand.
        if st.state == SyncState.Diverged && !includeDiverged then
          (Chunk((SyncAction("held back", st.path, "diverged — reconcile, or pass --include-diverged"), Some(st)))
            : Chunk[(SyncAction, Option[FileStatus])] < (Sync & Abort[Throwable]))
        else if !exportable || !safeRelative(st.path) then
          (Chunk.empty[(SyncAction, Option[FileStatus])]: Chunk[(SyncAction, Option[FileStatus])] < (Sync & Abort[Throwable]))
        else
          readBytes(sb.localFile(st.path)).map { bytes =>
            project(String(bytes, "UTF-8")) match
              case Left(err) => Chunk((SyncAction("refused", st.path, err), Some(st)))
              case Right(upstreamForm) =>
                val out = upstreamForm.getBytes("UTF-8")
                (if dryRun then ((): Unit < (Sync & Abort[Throwable])) else writeBytes(resolve(target, st.path), out))
                  .andThen(Chunk((SyncAction("wrote", st.path, st.state.label), None)))
          }
      }.map(_.flattenChunk)
    yield (results.map(_._1).toSeq, results.flatMap(_._2).toSeq)

  // ------------------------------------------------------------ index region

  /** Rewrites the bundle index below the marker, grouped by the mirror's top-level directories.
    *
    * Generated because forty hand-written bullets rot on the first `pull`.
    */
  def generateIndex(sb: SyncBundle, lock: SyncLock, today: LocalDate): Boolean < (Sync & Abort[Throwable]) =
    val file = sb.bundle.index.file
    file.read.map { text =>
      val normalized = text.replace("\r\n", "\n")
      val at = normalized.indexOf(IndexMarker)
      val preamble =
        if at >= 0 then normalized.take(at + IndexMarker.length)
        else normalized.stripTrailing() + "\n\n" + IndexMarker
      val updated = preamble + "\n\n" + renderIndexBody(sb, lock, today)
      if updated == normalized then false else file.write(updated).andThen(true)
    }

  private def renderIndexBody(sb: SyncBundle, lock: SyncLock, today: LocalDate): String =
    val sbuf = StringBuilder()
    sbuf ++= s"_Generated by `kb sync pull` — do not edit below the marker. Last built $today"
    if lock.baseCommit.nonEmpty then sbuf ++= s", from ${sb.manifest.repo}@${lock.baseCommit.take(8)}"
    sbuf ++= "._\n"
    val concepts = lock.files.filter(_.kind == SyncKind.Concept).sortBy(_.path)
    val assets = lock.files.filter(_.kind == SyncKind.Asset).sortBy(_.path)
    concepts.groupBy(e => e.path.split("/").take(3).mkString("/")).toSeq.sortBy(_._1).foreach { (group, items) =>
      sbuf ++= s"\n## $group\n\n"
      items.foreach { e =>
        val link = s"/${sb.manifest.root}/${e.path}"
        // The bullet text must equal the concept's own `description` verbatim — that equality is what
        // `index-description-drift` enforces, and generating the text from anything else would break it on import.
        val doc = sb.bundle.conceptAt(link)
        val title = doc.map(_.displayTitle).getOrElse(e.path.split("/").last)
        val description = doc.flatMap(_.fm.description).map(_.trim)
          .getOrElse(s"Upstream source document ${sb.manifest.repo}:${e.path}.")
        sbuf ++= s"* [$title]($link) - $description\n"
      }
    }
    if assets.nonEmpty then
      sbuf ++= s"\n## Assets (${assets.size})\n\n"
      sbuf ++= "Mirrored byte-for-byte and not concept documents — schemas, fixtures, examples, sidebar metadata.\n\n"
      assets.foreach(e => sbuf ++= s"* `${sb.manifest.root}/${e.path}`\n")
    sbuf.toString

  // ------------------------------------------------------------------ checks

  /** Findings a sync bundle owes, in the same shape as every other check so `kb check` renders them identically.
    *
    * Drift is a prompt, not a failure: only a damaged fence and a lockfile that disagrees with the disk are errors,
    * because those mean an export would send the wrong bytes.
    */
  def checkFindings(kb: Kb, sb: SyncBundle, upstreamRoot: Option[Path]): Seq[Finding] < (Sync & Abort[Throwable]) =
    status(sb, upstreamRoot).map { rows =>
      rows.flatMap { r =>
        val where = kb.rel(sb.localFile(r.path))
        r.state match
          case SyncState.Unreadable =>
            Seq(Finding(
              Severity.Error,
              "sync-projection-broken",
              where,
              Some(1),
              s"cannot reduce this file to its upstream form: ${r.detail}",
              Some(s"the `$FenceBegin` … `$FenceEnd` region is damaged; restore it or re-run `kb sync pull --theirs`")
            ))
          case SyncState.MissingLocal =>
            Seq(Finding(
              Severity.Error,
              "sync-lock-drift",
              where,
              None,
              "listed in sync.lock.yaml but absent from the mirror",
              Some("run `kb sync pull` to restore it, or `kb sync pull --prune` if upstream dropped it")
            ))
          case SyncState.Untracked =>
            Seq(Finding(
              Severity.Warn,
              "sync-untracked",
              where,
              None,
              "matches a manifest mapping but is not in sync.lock.yaml",
              Some("run `kb sync pull` to import it")
            ))
          case SyncState.Diverged =>
            Seq(Finding(
              Severity.Warn,
              "sync-diverged",
              where,
              None,
              "changed here and upstream since the last import",
              Some("reconcile by hand, then export; `kb sync diff` shows both sides")
            ))
          case SyncState.UpstreamOnly =>
            Seq(Finding(
              Severity.Warn,
              "sync-upstream-drift",
              where,
              None,
              "upstream has moved on since the last import",
              Some("run `kb sync pull` to take it — nothing here is lost, this file has no local edits")
            ))
          case SyncState.DeletedUpstream =>
            Seq(Finding(
              Severity.Warn,
              "sync-deleted-upstream",
              where,
              None,
              "no longer present upstream",
              Some("`kb sync pull --prune` removes it here too, if that is what you want")
            ))
          case SyncState.Clean | SyncState.LocalOnly => Nil
      }
    }

  // --------------------------------------------------------------- rendering

  def renderStatus(rows: Seq[FileStatus], json: Boolean, verbose: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "files" -> ujson.Arr.from(rows.map(r =>
            ujson.Obj("path" -> r.path, "kind" -> r.kind.label, "state" -> r.state.label, "detail" -> r.detail)
          )),
          "summary" -> ujson.Obj.from(
            rows.groupBy(_.state.label).map((k, v) => k -> ujson.Num(v.size.toDouble)).toSeq
          )
        ),
        indent = 2
      ) + "\n"
    else
      val sbuf = StringBuilder()
      val interesting = rows.filter(_.state != SyncState.Clean)
      val shown = if verbose then rows else interesting
      if shown.isEmpty then sbuf ++= s"${rows.size} file(s), all clean\n"
      else
        shown.sortBy(r => (r.state.label, r.path)).foreach { r =>
          sbuf ++= f"${r.state.label}%-17s ${r.path}"
          if r.detail.nonEmpty then sbuf ++= s"  — ${r.detail}"
          sbuf ++= "\n"
        }
        sbuf ++= "\n"
        rows.groupBy(_.state.label).toSeq.sortBy(_._1).foreach((k, v) => sbuf ++= s"$k: ${v.size}\n")
      sbuf.toString

  def renderActions(actions: Seq[SyncAction], refused: Seq[FileStatus], dryRun: Boolean, json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "dryRun" -> dryRun,
          "actions" -> ujson.Arr.from(actions.map(a =>
            ujson.Obj("verb" -> a.verb, "path" -> a.path, "detail" -> a.detail)
          )),
          "refused" -> ujson.Arr.from(refused.map(r =>
            ujson.Obj("path" -> r.path, "state" -> r.state.label, "detail" -> r.detail)
          ))
        ),
        indent = 2
      ) + "\n"
    else
      val sbuf = StringBuilder()
      if actions.isEmpty && refused.isEmpty then sbuf ++= "nothing to do\n"
      actions.groupBy(_.verb).toSeq.sortBy(_._1).foreach { (verb, items) =>
        val label = if dryRun then s"would $verb" else verb
        sbuf ++= s"$label (${items.size})\n"
        items.sortBy(_.path).foreach(a => sbuf ++= s"  ${a.path}\n")
      }
      if refused.nonEmpty then
        sbuf ++= s"\nrefused (${refused.size}) — resolve by hand, or re-run with --theirs to take upstream\n"
        refused.sortBy(_.path).foreach { r =>
          sbuf ++= s"  ${r.path}  [${r.state.label}]"
          if r.detail.nonEmpty then sbuf ++= s" — ${r.detail}"
          sbuf ++= "\n"
        }
      sbuf.toString
