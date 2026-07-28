//| scalaVersion: 3.8.4
//| moduleDeps: [KbIntent.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Writing intent: creating records, moving them between states, and regenerating the grouped index.
  *
  * Transitions demand their obligations up front — `release` requires a capability, `cancel` a reason — so the answer
  * is captured while it is still in your head, rather than surfacing as a check failure days later.
  */

import kyo.*
import java.time.LocalDate

object KbIntentEdit:

  // ------------------------------------------------------- frontmatter edits

  /** Sets or removes top-level scalar frontmatter keys, leaving everything else byte-for-byte alone.
    *
    * Only column-zero `key:` lines are considered, so nested structures (`generated:`, `sources:`) are never touched.
    */
  def setKeys(file: Path, updates: Seq[(String, Option[String])]): Unit < (Sync & Abort[Throwable]) =
    file.read.map { text =>
      val (rawFm, body) = KbStore.splitFrontmatter(text)
      rawFm match
        case None => Abort.fail(RuntimeException(s"${KbPath.render(file)} has no frontmatter to edit"))
        case Some(fm) =>
          val lines = fm.split("\n", -1).toVector
          var out = lines
          updates.foreach { (key, value) =>
            val idx = out.indexWhere(l => l.startsWith(s"$key:"))
            (idx, value) match
              case (-1, Some(v)) =>
                // Append after the last top-level scalar so new keys do not land inside a nested block.
                val at = out.lastIndexWhere(l => l.nonEmpty && !l.startsWith(" ") && !l.startsWith("-")) + 1
                out = out.take(at) ++ Vector(s"$key: $v") ++ out.drop(at)
              case (-1, None) => ()
              case (i, Some(v)) => out = out.updated(i, s"$key: $v")
              case (i, None) =>
                // Drop the key and any indented continuation beneath it.
                val end = out.indexWhere(l => l.nonEmpty && !l.startsWith(" "), i + 1) match
                  case -1 => out.length
                  case n => n
                out = out.take(i) ++ out.drop(end)
          }
          file.write(s"---\n${out.mkString("\n").stripTrailing()}\n---\n$body")
    }

  private def yamlStr(s: String): String =
    val needsQuote = s.exists(c => ":#{}[]&*!|>'\"%@`,".contains(c)) || s.startsWith(" ") || s.endsWith(" ")
    if needsQuote then "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"" else s

  // ---------------------------------------------------------------- creating

  def create(
      b: Bundle,
      title: String,
      description: String,
      kind: IntentKind,
      breaking: Boolean,
      issue: Option[String],
      tags: Seq[String],
      today: LocalDate
  ): Either[String, Path] < (Sync & Abort[Throwable]) =
    val id = KbIntent.nextId(b)
    val slug = KbScaffold.slugify(title)
    val file = b.root / s"$id-$slug.md"
    file.exists.map {
      case true => Left(s"${KbPath.render(file)} already exists")
      case false =>
        val sb = StringBuilder()
        sb ++= "---\n"
        sb ++= "type: Intent\n"
        sb ++= s"title: ${yamlStr(title)}\n"
        sb ++= s"description: ${yamlStr(description)}\n"
        sb ++= s"state: ${IntentState.Backlog}\n"
        sb ++= s"kind: ${kind.label}\n"
        sb ++= s"breaking: $breaking\n"
        sb ++= s"created: $today\n"
        sb ++= s"state_since: $today\n"
        issue.foreach(x => sb ++= s"issue: ${x.stripPrefix("#")}\n")
        if tags.nonEmpty then sb ++= s"tags: [${tags.map(KbScaffold.slugify).mkString(", ")}]\n"
        sb ++= "---\n\n"
        sb ++= s"# $id — $title\n\n"
        sb ++= s"$description\n\n"
        sb ++= "## Problem\n\n<!-- TODO: what problem is this solving? Resist describing a solution here. -->\n\n"
        sb ++= "## Approach\n\n<!-- TODO: fill in during Refinement. Delete if it stays trivial. -->\n"
        file.write(sb.toString).andThen(Right(file))
    }

  // ------------------------------------------------------------- transitions

  case class Transition(
      to: IntentState,
      capability: Option[String] = None,
      artifacts: Seq[String] = Nil,
      reason: Option[String] = None,
      supersededBy: Option[String] = None
  )

  /** Applies a transition, refusing up front when the target state's obligation is unmet. */
  def transition(
      kb: Kb,
      b: Bundle,
      i: Intent,
      t: Transition,
      today: LocalDate
  ): Either[String, Path] < (Sync & Abort[Throwable]) =
    val missing = t.to match
      // Only user-visible kinds owe a capability; internal work (build, refactor, test) often changes nothing a
      // reader of the knowledge base needs to know.
      case IntentState.Released if t.capability.isEmpty && i.capability.isEmpty && i.kind.forall(_.userVisible) =>
        val target = if i.kind.contains(IntentKind.Spike) then "design note" else "capability"
        Some(s"releasing needs --capability bundle:/path.md (the $target this produced)")
      case IntentState.Cancelled if t.reason.isEmpty && i.reason.isEmpty =>
        Some("cancelling needs --reason")
      case IntentState.Superseded if t.supersededBy.isEmpty && i.supersededBy.isEmpty =>
        Some("superseding needs --by <intent-id>")
      case _ => None

    val badRef = t.capability.flatMap { raw =>
      DocRef.parse(raw) match
        case None => Some(s"`$raw` is not `bundle-label:/path.md`")
        case Some(ref) => KbIntent.resolveRef(kb, ref)
    }

    val badSuccessor = t.supersededBy.flatMap { s =>
      Option.when(KbIntent.find(b, s).isEmpty)(s"no intent `$s` in ${b.label}")
    }

    (missing orElse badRef orElse badSuccessor) match
      case Some(err) => (Left(err): Either[String, Path] < (Sync & Abort[Throwable]))
      case None =>
        val updates = Seq(
          "state" -> Some(t.to.toString),
          "state_since" -> Some(today.toString)
        ) ++
          t.capability.map(c => "capability" -> Some(c)) ++
          t.reason.map(r => "reason" -> Some(yamlStr(r))) ++
          t.supersededBy.map(s => "superseded_by" -> Some(s)) ++
          Option.when(t.artifacts.nonEmpty)("artifacts" -> Some(t.artifacts.mkString("[", ", ", "]")))
        setKeys(i.doc.file, updates).andThen(Right(i.doc.file))

  // ------------------------------------------------------- index generation

  /** Rewrites the intent bundle's index below the marker, grouped by state.
    *
    * Generated rather than hand-maintained, which is what stops it rotting — and it is the human-readable answer to
    * "what is pending, in flight, and no longer valid".
    */
  def generateIndex(b: Bundle, today: LocalDate): Boolean < (Sync & Abort[Throwable]) =
    b.index.file.read.map { text =>
      val normalized = text.replace("\r\n", "\n")
      val markerAt = normalized.indexOf(KbIntent.Marker)
      val preamble =
        if markerAt >= 0 then normalized.take(markerAt + KbIntent.Marker.length)
        else normalized.stripTrailing() + "\n\n" + KbIntent.Marker
      val generated = renderSections(KbIntent.intents(b), today)
      val updated = preamble + "\n\n" + generated
      if updated == normalized then false
      else b.index.file.write(updated).andThen(true)
    }

  private def renderSections(items: Seq[Intent], today: LocalDate): String =
    val sb = StringBuilder()
    sb ++= s"_Generated by `kb refresh` — do not edit below the marker. Last built $today._\n"
    IntentState.displayOrder.foreach { st =>
      val group = items.filter(_.state.contains(st)).sortBy(_.id)
      if group.nonEmpty then
        sb ++= s"\n## ${heading(st)} (${group.size})\n\n"
        // Flags live in the link text, never after the description: the bullet's trailing text must stay verbatim
        // equal to the concept's `description`, which is what `kb check` enforces.
        group.foreach(i => sb ++= s"* [${linkText(i)}](${i.doc.bundlePath}) - ${i.description.getOrElse("").trim}\n")
    }
    val stateless = items.filter(_.state.isEmpty)
    if stateless.nonEmpty then
      sb ++= s"\n## Without a state (${stateless.size})\n\n"
      stateless.foreach(i => sb ++= s"* [${linkText(i)}](${i.doc.bundlePath}) - ${i.description.getOrElse("").trim}\n")
    if items.isEmpty then sb ++= "\nNo intent recorded yet. `kb intent new --title … --description … --kind feature`\n"
    sb.toString

  private def linkText(i: Intent): String =
    val flags = (if i.breaking then Seq("breaking") else Nil) ++ i.kind.map(_.label).toSeq
    if flags.isEmpty then s"${i.id} ${i.title}" else s"${i.id} ${i.title} — ${flags.mkString(", ")}"

  private def heading(st: IntentState): String = st match
    case IntentState.InProgress => "In progress"
    case IntentState.Backlog => "Backlog"
    case IntentState.Refinement => "In refinement"
    case IntentState.Released => "Released"
    case IntentState.Cancelled => "Cancelled"
    case IntentState.Superseded => "Superseded"

  // -------------------------------------------------------------------- init

  /** Scaffolds an intent bundle in a knowledge base that has none. */
  def initBundle(
      kbRoot: Path,
      name: String,
      system: Option[String],
      capabilityBundle: Option[String],
      staleAfterDays: Int,
      today: LocalDate
  ): Either[String, Seq[Path]] < (Sync & Abort[Throwable]) =
    val dir = kbRoot / "bundles" / KbScaffold.slugify(name)
    dir.exists.map {
      case true => Left(s"${KbPath.render(dir)} already exists")
      case false =>
        val index = dir / "index.md"
        val log = dir / "log.md"
        val sb = StringBuilder()
        sb ++= "---\n"
        sb ++= "okf_version: \"0.2\"\n"
        sb ++= "title: Intent\n"
        sb ++= "description: \"Work this project means to do, is doing, or has done — with the reasoning behind it.\"\n"
        sb ++= "intent: true\n"
        system.foreach(s => sb ++= s"system: $s\n")
        capabilityBundle.foreach(c => sb ++= s"capability_bundle: $c\n")
        sb ++= s"stale_after_days: $staleAfterDays\n"
        sb ++= "---\n\n"
        sb ++= "# Intent\n\n"
        sb ++= "Work this project means to do, is doing, or has done — with the reasoning behind it.\n\n"
        sb ++= "Each entry is an Intent: future-tense, with a lifecycle. What the system actually *does* today lives\n"
        sb ++= "in the capability bundle, in the present tense. Releasing an Intent requires linking the Capability it\n"
        sb ++= "produced, which is what keeps the two in step.\n\n"
        sb ++= s"${KbIntent.Marker}\n"
        for
          _ <- index.write(sb.toString)
          _ <- log.write(s"# Log\n\n## $today\n\n* **Creation**: Intent bundle created.\n")
        yield Right(Seq(index, log))
    }
