//| scalaVersion: 3.8.4
//| moduleDeps: [KbScaffold.scala, KbCheck.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Intent management — features, enhancements and bugs recorded as prose with a lifecycle.
  *
  * The distinction this rests on: an **Intent** is future-tense and has a lifecycle; a **Capability** is present-tense
  * and is simply either true or stale. Releasing is where they meet, which is why marking an Intent Released demands
  * a link to the Capability it produced. See kb/CONTEXT.md for the glossary and docs/adr/0001 for why.
  *
  * Obligations are checked wherever a record currently sits, never against the path it took to get there — work
  * genuinely jumps stages, and a tool that fights that gets worked around.
  */

import kyo.*
import java.time.LocalDate
import java.time.temporal.ChronoUnit

enum IntentState:
  case Backlog, Refinement, InProgress, Released, Cancelled, Superseded

  def isTerminal: Boolean = this match
    case Released | Cancelled | Superseded => true
    case _ => false

  /** States where nothing moving is a real signal. Backlog is excluded — a backlog is *meant* to sit. */
  def isActive: Boolean = this match
    case Refinement | InProgress => true
    case _ => false

object IntentState:
  val all: Seq[IntentState] = values.toSeq
  def parse(s: String): Option[IntentState] = all.find(_.toString.equalsIgnoreCase(s.trim.replace("-", "")))
  /** Display order: live work first, then terminal records. */
  val displayOrder: Seq[IntentState] =
    Seq(InProgress, Refinement, Backlog, Released, Superseded, Cancelled)

enum IntentKind(val userVisible: Boolean):
  case Feature extends IntentKind(true)
  case Bug extends IntentKind(true)
  case Performance extends IntentKind(true)
  case Security extends IntentKind(true)
  case Deprecation extends IntentKind(true)
  case Removal extends IntentKind(true)
  case Refactor extends IntentKind(false)
  case Docs extends IntentKind(false)
  case Test extends IntentKind(false)
  case Build extends IntentKind(false)
  case Spike extends IntentKind(false)

  def label: String = toString.toLowerCase

object IntentKind:
  val all: Seq[IntentKind] = values.toSeq
  def parse(s: String): Option[IntentKind] = all.find(_.label == s.trim.toLowerCase)
  def names: String = all.map(_.label).mkString(", ")

/** A reference to a document elsewhere in the knowledge base: `bundle-label:/path.md`.
  *
  * Deliberately not a Package URL — purl identifies registry-backed artifacts, and a Capability is a markdown
  * document no registry knows about. See docs/adr/0003.
  */
case class DocRef(bundle: String, path: String):
  def render: String = s"$bundle:$path"

object DocRef:
  private val Pattern = raw"^([^:\s]+):(/[^\s]*)$$".r
  def parse(s: String): Option[DocRef] = s.trim match
    case Pattern(b, p) => Some(DocRef(b, p))
    case _ => None

/** Settings declared in the intent bundle's own index frontmatter, so the skill is portable. */
case class IntentConfig(
    system: Option[String],
    capabilityBundle: Option[String],
    staleAfterDays: Int
)

object IntentConfig:
  val DefaultStaleAfterDays = 60
  def from(fm: Frontmatter): IntentConfig =
    IntentConfig(
      system = fm.str("system"),
      capabilityBundle = fm.str("capability_bundle"),
      staleAfterDays = fm.str("stale_after_days").flatMap(_.trim.toIntOption).getOrElse(DefaultStaleAfterDays)
    )

case class Intent(doc: Doc):
  private def fm = doc.fm
  def id: String = doc.name.takeWhile(_.isDigit)
  def slug: String = doc.name.stripSuffix(".md")
  def title: String = doc.displayTitle
  def description: Option[String] = fm.description
  def state: Option[IntentState] = fm.str("state").flatMap(IntentState.parse)
  def kind: Option[IntentKind] = fm.str("kind").flatMap(IntentKind.parse)
  def breaking: Boolean = fm.values.get("breaking").exists {
    case b: java.lang.Boolean => b.booleanValue
    case s: String => s.equalsIgnoreCase("true")
    case _ => false
  }
  def created: Option[LocalDate] = date("created")
  def stateSince: Option[LocalDate] = date("state_since")
  def issue: Option[String] = fm.str("issue")
  def capability: Option[String] = fm.str("capability")
  def supersededBy: Option[String] = fm.str("superseded_by")
  def reason: Option[String] = fm.str("reason")
  def artifacts: List[String] = fm.strList("artifacts")

  private def date(key: String): Option[LocalDate] =
    fm.str(key).flatMap(s => try Some(LocalDate.parse(s.trim)) catch case _: Exception => None)

  def daysSinceStateChange(today: LocalDate): Option[Long] =
    stateSince.map(d => ChronoUnit.DAYS.between(d, today))

object KbIntent:

  val Marker = "<!-- intent:index -->"

  /** The intent bundle is the one whose index frontmatter carries `intent: true`. Nothing is hardcoded, so the skill
    * works in any repository whatever the bundle is called.
    */
  def findBundle(kb: Kb): Option[Bundle] =
    kb.bundles.find(_.index.fm.values.get("intent").exists {
      case b: java.lang.Boolean => b.booleanValue
      case s: String => s.equalsIgnoreCase("true")
      case _ => false
    })

  def config(b: Bundle): IntentConfig = IntentConfig.from(b.index.fm)

  def intents(b: Bundle): Seq[Intent] =
    b.concepts.filter(_.fm.`type`.exists(_.equalsIgnoreCase("Intent"))).map(Intent(_)).sortBy(_.slug)

  def nextId(b: Bundle): String =
    val used = intents(b).flatMap(_.id.toIntOption)
    f"${(used.maxOption.getOrElse(0) + 1)}%04d"

  def find(b: Bundle, idOrSlug: String): Option[Intent] =
    val needle = idOrSlug.trim
    intents(b).find(i => i.id == needle || i.slug == needle || i.id == f"${needle.toIntOption.getOrElse(-1)}%04d")

  // -------------------------------------------------------------------- check

  /** Obligations owed by each state, plus staleness. Reuses the kb Finding type so `kb check` and `kb intent check`
    * render identically.
    */
  def check(kb: Kb, b: Bundle, today: LocalDate): Seq[Finding] =
    val cfg = config(b)
    val all = intents(b)
    val byId = all.map(i => i.id -> i).toMap
    all.flatMap(checkOne(kb, b, cfg, byId, _, today)) ++ checkBundle(kb, b, cfg)

  private def checkBundle(kb: Kb, b: Bundle, cfg: IntentConfig): Seq[Finding] =
    val where = kb.rel(b.index.file)
    Seq(
      Option.when(cfg.system.isEmpty)(
        Finding(Severity.Warn, "intent-no-system", where, Some(1),
          "intent bundle declares no `system`",
          Some("add a Package URL, e.g. system: pkg:maven/org.finos.morphir/morphir-core"))
      ),
      Option.when(cfg.capabilityBundle.isEmpty)(
        Finding(Severity.Warn, "intent-no-capability-bundle", where, Some(1),
          "intent bundle declares no `capability_bundle`",
          Some("releasing cannot be checked against a target bundle without it"))
      )
    ).flatten

  private def checkOne(
      kb: Kb,
      b: Bundle,
      cfg: IntentConfig,
      byId: Map[String, Intent],
      i: Intent,
      today: LocalDate
  ): Seq[Finding] =
    val where = kb.rel(i.doc.file)
    def err(check: String, msg: String, hint: Option[String] = None) =
      Finding(Severity.Error, check, where, Some(1), msg, hint)
    def warn(check: String, msg: String, hint: Option[String] = None) =
      Finding(Severity.Warn, check, where, Some(1), msg, hint)

    val shape = Seq(
      Option.when(i.state.isEmpty)(
        err("intent-state-missing",
          i.doc.fm.str("state").fold("intent has no `state`")(s => s"`state: $s` is not a known state"),
          Some(s"one of ${IntentState.all.mkString(", ")}"))
      ),
      Option.when(i.kind.isEmpty)(
        err("intent-kind-missing",
          i.doc.fm.str("kind").fold("intent has no `kind`")(s => s"`kind: $s` is not a known kind"),
          Some(s"one of ${IntentKind.names}"))
      ),
      Option.when(i.created.isEmpty)(err("intent-created-missing", "intent has no valid `created` date (YYYY-MM-DD)")),
      Option.when(i.stateSince.isEmpty)(
        err("intent-state-since-missing", "intent has no valid `state_since` date (YYYY-MM-DD)",
          Some("it is what staleness is measured from"))
      )
    ).flatten

    val obligations = i.state.toSeq.flatMap {
      case IntentState.Released =>
        val target = if i.kind.contains(IntentKind.Spike) then "Design Note" else "Capability"
        i.capability match
          case None if i.kind.forall(_.userVisible) =>
            // User-visible work must teach the knowledge base what changed. Internal work often has nothing to
            // teach it, and inventing a document for "added three labels" is the noise this design avoids.
            Seq(err("intent-released-no-capability",
              s"Released intent does not link to the $target it produced",
              Some("kb intent release <id> --capability bundle:/path.md")))
          case None =>
            Seq(warn("intent-released-no-capability-internal",
              s"Released ${i.kind.map(_.label).getOrElse("")} intent links to no $target",
              Some("fine when there is nothing for the knowledge base to learn; link one if there is")))
          case Some(raw) =>
            DocRef.parse(raw) match
              case None =>
                Seq(err("intent-capability-malformed", s"`capability: $raw` is not `bundle-label:/path.md`"))
              case Some(ref) => resolveRef(kb, ref).toSeq.map(err("intent-capability-unresolved", _))
      case IntentState.Cancelled =>
        Option.when(i.reason.forall(_.trim.isEmpty))(
          err("intent-cancelled-no-reason", "Cancelled intent has no `reason`",
            Some("a cancellation without a reason is worthless six months on"))
        ).toSeq
      case IntentState.Superseded =>
        i.supersededBy match
          case None =>
            Seq(err("intent-superseded-no-successor", "Superseded intent has no `superseded_by`"))
          case Some(succ) if !byId.contains(f"${succ.trim.toIntOption.getOrElse(-1)}%04d") && !byId.contains(succ.trim) =>
            Seq(err("intent-superseded-unknown", s"`superseded_by: $succ` names no intent in this bundle"))
          case _ => Nil
      case _ => Nil
    }

    val staleness = for
      st <- i.state.toSeq if st.isActive
      days <- i.daysSinceStateChange(today).toSeq if days > cfg.staleAfterDays
    yield warn("intent-stale", s"in $st for $days days (threshold ${cfg.staleAfterDays})",
      Some("move it on, or move it back to Backlog and say so"))

    val purl = i.artifacts.filterNot(_.trim.startsWith("pkg:")).map { a =>
      warn("intent-artifact-not-purl", s"artifact `$a` is not a Package URL", Some("expected pkg:type/namespace/name@version"))
    }

    shape ++ obligations ++ staleness ++ purl

  /** None when the reference resolves; Some(message) when it does not. */
  def resolveRef(kb: Kb, ref: DocRef): Option[String] =
    kb.bundle(ref.bundle) match
      case None => Some(s"no bundle `${ref.bundle}` (known: ${kb.bundles.map(_.label).mkString(", ")})")
      case Some(target) =>
        Option.when(target.conceptAt(ref.path).isEmpty)(s"`${ref.render}` names no concept in ${target.label}")

  // ---------------------------------------------------------------- rendering

  def renderList(kb: Kb, b: Bundle, items: Seq[Intent], json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "bundle" -> b.label,
          "count" -> items.size,
          "intents" -> ujson.Arr.from(items.map(toJson))
        ),
        indent = 2
      ) + "\n"
    else if items.isEmpty then "no matching intent\n"
    else
      val sb = StringBuilder()
      IntentState.displayOrder.foreach { st =>
        val group = items.filter(_.state.contains(st))
        if group.nonEmpty then
          sb ++= s"\n${st.toString.toUpperCase} (${group.size})\n"
          group.foreach { i =>
            val flags = (if i.breaking then Seq("breaking") else Nil) ++ i.kind.map(_.label).toSeq
            sb ++= f"  ${i.id}%-6s ${i.title}%-48s ${flags.mkString(", ")}\n"
          }
      }
      val orphan = items.filter(_.state.isEmpty)
      if orphan.nonEmpty then
        sb ++= s"\nNO STATE (${orphan.size})\n"
        orphan.foreach(i => sb ++= s"  ${i.id}   ${i.title}\n")
      sb ++= s"\n${items.size} intent\n"
      sb.toString

  private def toJson(i: Intent): ujson.Obj =
    ujson.Obj(
      "id" -> i.id,
      "slug" -> i.slug,
      "path" -> i.doc.bundlePath,
      "title" -> i.title,
      "description" -> i.description.map(ujson.Str(_)).getOrElse(ujson.Null),
      "state" -> i.state.map(s => ujson.Str(s.toString)).getOrElse(ujson.Null),
      "kind" -> i.kind.map(k => ujson.Str(k.label)).getOrElse(ujson.Null),
      "userVisible" -> i.kind.map(k => ujson.Bool(k.userVisible)).getOrElse(ujson.Null),
      "breaking" -> i.breaking,
      "created" -> i.created.map(d => ujson.Str(d.toString)).getOrElse(ujson.Null),
      "stateSince" -> i.stateSince.map(d => ujson.Str(d.toString)).getOrElse(ujson.Null),
      "issue" -> i.issue.map(ujson.Str(_)).getOrElse(ujson.Null),
      "capability" -> i.capability.map(ujson.Str(_)).getOrElse(ujson.Null),
      "supersededBy" -> i.supersededBy.map(ujson.Str(_)).getOrElse(ujson.Null),
      "artifacts" -> ujson.Arr.from(i.artifacts.map(ujson.Str(_)))
    )

  def renderShow(kb: Kb, i: Intent, json: Boolean): String =
    if json then ujson.write(toJson(i), indent = 2) + "\n"
    else
      val sb = StringBuilder()
      sb ++= s"intent ${i.id} — ${i.title}\n"
      i.description.foreach(d => sb ++= s"$d\n")
      sb ++= s"\nstate        ${i.state.map(_.toString).getOrElse("(missing)")}"
      i.stateSince.foreach(d => sb ++= s"  since $d")
      sb ++= "\n"
      sb ++= s"kind         ${i.kind.map(_.label).getOrElse("(missing)")}"
      if i.breaking then sb ++= "  BREAKING"
      sb ++= "\n"
      i.created.foreach(d => sb ++= s"created      $d\n")
      i.issue.foreach(x => sb ++= s"issue        #$x\n")
      i.capability.foreach(c => sb ++= s"capability   $c\n")
      i.supersededBy.foreach(x => sb ++= s"superseded   by $x\n")
      i.reason.foreach(r => sb ++= s"reason       $r\n")
      if i.artifacts.nonEmpty then sb ++= s"artifacts    ${i.artifacts.mkString("\n             ")}\n"
      sb ++= s"file         ${kb.rel(i.doc.file)}\n"
      sb.toString
