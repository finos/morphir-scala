//| scalaVersion: 3.8.4
//| moduleDeps: [KbScaffold.scala, KbIndex.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Bringing derived state back in line with the markdown.
  *
  * Two kinds of derived state exist in this knowledge base:
  *
  *   - the **markdown indexes**, whose bullets mirror each concept's `description`
  *   - the **SQLite index**, which is recomputed from the files
  *
  * `refresh` reconciles both. It only makes changes that are mechanical — rewriting a bullet to match the
  * description it is supposed to mirror, or rebuilding a stale database. Adding a missing entry means choosing which
  * section it belongs under, which is a judgement call, so that is opt-in.
  */

import kyo.*

enum RefreshKind:
  case DescriptionFixed, EntryAdded, EntryMissing, IndexRebuilt, IndexFresh

case class RefreshAction(kind: RefreshKind, file: String, detail: String)

object KbRefresh:

  private def normalize(s: String): String =
    s.trim.stripSuffix(".").replaceAll("\\s+", " ").toLowerCase

  /** Rewrites index bullets whose text has drifted from the concept's `description`.
    *
    * Only the trailing description is touched; the link itself is preserved verbatim, so a hand-written link title
    * survives a refresh.
    */
  def refreshMarkdown(
      kb: Kb,
      addMissing: Boolean,
      section: String,
      dryRun: Boolean
  ): Seq[RefreshAction] < (Sync & Abort[Throwable]) =
    Kyo.foreach(Chunk.from(kb.bundles))(refreshBundle(kb, _, addMissing, section, dryRun)).map(_.flatten.toSeq)

  private def refreshBundle(
      kb: Kb,
      b: Bundle,
      addMissing: Boolean,
      section: String,
      dryRun: Boolean
  ): Chunk[RefreshAction] < (Sync & Abort[Throwable]) =
    for
      fixes <- Kyo.foreach(Chunk.from(b.allIndexes))(fixIndex(kb, b, _, dryRun)).map(_.flattenChunk)
      missing <- addMissingEntries(kb, b, addMissing, section, dryRun)
    yield fixes ++ missing

  private def fixIndex(kb: Kb, b: Bundle, idx: Doc, dryRun: Boolean): Chunk[RefreshAction] < (Sync & Abort[Throwable]) =
    idx.file.read.map { text =>
      val lines = text.replace("\r\n", "\n").linesIterator.toVector
      val actions = collection.mutable.ArrayBuffer.empty[RefreshAction]
      val updated = lines.zipWithIndex.map { (line, i) =>
        KbMarkdown.IndexEntry.findFirstMatchIn(line) match
          case Some(m) =>
            val dest = m.group(3)
            val current = Option(m.group(4)).map(_.trim).getOrElse("")
            if !dest.startsWith("/") then line
            else
              b.conceptAt(dest.takeWhile(_ != '#')).flatMap(_.fm.description).map(_.trim) match
                case Some(want) if normalize(current) != normalize(want) =>
                  actions += RefreshAction(
                    RefreshKind.DescriptionFixed,
                    s"${kb.rel(idx.file)}:${i + 1}",
                    s"$dest — was ${if current.isEmpty then "(no description)" else s"\"$current\""}"
                  )
                  s"${m.group(1)} - $want"
                case _ => line
          case None => line
      }
      if actions.isEmpty || dryRun then Chunk.from(actions.toSeq)
      else idx.file.write(updated.mkString("\n").stripTrailing() + "\n").andThen(Chunk.from(actions.toSeq))
    }

  /** Concepts no index links to. Reported always; appended only when `addMissing` is set. */
  private def addMissingEntries(
      kb: Kb,
      b: Bundle,
      addMissing: Boolean,
      section: String,
      dryRun: Boolean
  ): Chunk[RefreshAction] < (Sync & Abort[Throwable]) =
    val linked = b.allIndexes.flatMap(_.links.filter(_.isBundleRelative).map(_.dest.takeWhile(_ != '#'))).toSet
    val missing = b.concepts.filterNot(c => linked.contains(c.bundlePath))
    if missing.isEmpty then (Chunk.empty[RefreshAction]: Chunk[RefreshAction] < (Sync & Abort[Throwable]))
    else if !addMissing then
      Chunk.from(missing.map { c =>
        RefreshAction(
          RefreshKind.EntryMissing,
          kb.rel(c.file),
          s"not linked from any index in ${b.label} — pass --add-missing to append it"
        )
      })
    else
      Kyo.foreach(Chunk.from(missing)) { c =>
        // Subdirectory concepts belong in that subdirectory's index when one exists.
        val idxDoc = b.subIndexes
          .find(i => c.rel.length > 1 && i.rel.dropRight(1) == c.rel.dropRight(1))
          .getOrElse(b.index)
        val action = RefreshAction(
          RefreshKind.EntryAdded,
          kb.rel(idxDoc.file),
          s"${c.bundlePath} under \"$section\""
        )
        if dryRun then (action: RefreshAction < (Sync & Abort[Throwable]))
        else
          KbScaffold
            .insertIndexEntry(idxDoc.file, section, c.displayTitle, c.bundlePath, c.fm.description.getOrElse(""))
            .andThen(action)
      }

  /** Rebuilds the SQLite index when it is missing, stale, or `force` is set. */
  def refreshDb(kb: Kb, db: Path, force: Boolean, dryRun: Boolean): Seq[RefreshAction] < (Sync & Abort[Throwable]) =
    KbIndex.status(db, kb).map {
      case Left(_) =>
        // No index yet, or one without usable metadata: build it.
        if dryRun then Seq(RefreshAction(RefreshKind.IndexRebuilt, KbPath.render(db), "would build (absent)"))
        else KbIndex.build(kb, db).map(s => Seq(RefreshAction(RefreshKind.IndexRebuilt, KbPath.render(db), s"built ${s.docs} docs")))
      case Right((builtAt, _, stale)) =>
        if stale.isEmpty && !force then
          (Seq(RefreshAction(RefreshKind.IndexFresh, KbPath.render(db), s"up to date (built $builtAt)")): Seq[RefreshAction] < (Sync & Abort[Throwable]))
        else
          val why = if force then "forced" else s"${stale.size} file(s) changed"
          if dryRun then Seq(RefreshAction(RefreshKind.IndexRebuilt, KbPath.render(db), s"would rebuild ($why)"))
          else KbIndex.build(kb, db).map(s => Seq(RefreshAction(RefreshKind.IndexRebuilt, KbPath.render(db), s"rebuilt ${s.docs} docs ($why)")))
    }

  // -------------------------------------------------------------- rendering

  def render(actions: Seq[RefreshAction], dryRun: Boolean, json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "dryRun" -> dryRun,
          "changed" -> actions.count(a => a.kind != RefreshKind.IndexFresh && a.kind != RefreshKind.EntryMissing),
          "actions" -> ujson.Arr.from(actions.map { a =>
            ujson.Obj("kind" -> a.kind.toString, "file" -> a.file, "detail" -> a.detail)
          })
        ),
        indent = 2
      ) + "\n"
    else
      val sb = StringBuilder()
      actions.foreach { a =>
        val verb = a.kind match
          case RefreshKind.DescriptionFixed => if dryRun then "would fix " else "fixed    "
          case RefreshKind.EntryAdded => if dryRun then "would add " else "added    "
          case RefreshKind.EntryMissing => "missing  "
          case RefreshKind.IndexRebuilt => if dryRun then "would build" else "rebuilt  "
          case RefreshKind.IndexFresh => "fresh    "
        sb ++= s"$verb ${a.file}\n           ${a.detail}\n"
      }
      val fixed = actions.count(_.kind == RefreshKind.DescriptionFixed)
      val added = actions.count(_.kind == RefreshKind.EntryAdded)
      val missing = actions.count(_.kind == RefreshKind.EntryMissing)
      if actions.isEmpty then sb ++= "nothing to refresh\n"
      sb ++= s"\n$fixed description(s) ${if dryRun then "to fix" else "fixed"}"
      if added > 0 then sb ++= s", $added entr${if added == 1 then "y" else "ies"} ${if dryRun then "to add" else "added"}"
      if missing > 0 then sb ++= s", $missing unindexed concept(s)"
      sb ++= "\n"
      sb.toString
