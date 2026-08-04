//| scalaVersion: 3.8.4
//| moduleDeps: [KbStore.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Decision Records — architectural decisions recorded as prose, with supersession rather than revision.
  *
  * The distinction this rests on, and why it is a third register alongside the two the knowledge base already had:
  *
  *   - An **Intent** is future-tense and has a lifecycle. It answers *should we do this*.
  *   - A **Capability** is present-tense and has no lifecycle. It answers *what does the system do*.
  *   - A **Decision Record** is past-tense and immutable. It answers *why is it shaped this way* — including which
  *     alternatives were rejected and under what condition the decision should be revisited.
  *
  * The immutability is the point, and it is what separates a Decision Record from a Design Note. A Design Note is
  * updated as understanding improves. A Decision Record is not: once made, it is superseded by a later record rather
  * than edited, so that the reasoning available at the time survives even after the conclusion changes. That is why
  * `supersedes`/`superseded_by` are modelled here and checked for mutual consistency.
  *
  * Records are found by `type: Decision Record` wherever they sit — no bundle marker, no configuration. Ids come from
  * the filename prefix, matching how intent ids work, so the id and the file can never disagree.
  */

import kyo.*
import java.time.LocalDate

enum DecisionState:
  case Proposed, Accepted, Superseded, Withdrawn

  /** A decision that no longer governs. Readers should follow it to whatever replaced it. */
  def isRetired: Boolean = this match
    case Superseded | Withdrawn => true
    case _ => false

object DecisionState:
  val all: Seq[DecisionState] = values.toSeq
  def parse(s: String): Option[DecisionState] = all.find(_.toString.equalsIgnoreCase(s.trim))
  def names: String = all.map(_.toString).mkString(", ")
  /** Display order: what governs now, then what used to. */
  val displayOrder: Seq[DecisionState] = Seq(Accepted, Proposed, Superseded, Withdrawn)

/** The `type` value that marks a concept as a decision record. */
object DecisionType:
  val Name = "Decision Record"

case class Decision(doc: Doc, bundle: String):
  private def fm = doc.fm
  /** Leading digits of the filename, e.g. `0004` from `0004-bridge-nothing.md`. Empty when there are none. */
  def id: String = doc.name.takeWhile(_.isDigit)
  def slug: String = doc.name.stripSuffix(".md")
  def title: String = doc.displayTitle
  def description: Option[String] = fm.description
  def state: Option[DecisionState] = fm.str("state").flatMap(DecisionState.parse)
  def decided: Option[LocalDate] = date("decided")
  def supersedes: List[String] = fm.strList("supersedes").map(normalizeId)
  def supersededBy: Option[String] = fm.str("superseded_by").map(normalizeId)
  def reason: Option[String] = fm.str("reason")
  def tags: List[String] = fm.tags

  private def date(key: String): Option[LocalDate] =
    fm.str(key).flatMap(s => try Some(LocalDate.parse(s.trim)) catch case _: Exception => None)

  /** Accepts `4`, `0004` and `0004-some-slug` alike, so a human writing `supersedes: [2]` is not punished. */
  private def normalizeId(raw: String): String =
    val head = raw.trim.takeWhile(_.isDigit)
    if head.isEmpty then raw.trim else f"${head.toInt}%04d"

object KbDecision:

  def decisions(kb: Kb): Seq[Decision] =
    kb.concepts.collect {
      case (b, d) if d.fm.`type`.exists(_.equalsIgnoreCase(DecisionType.Name)) => Decision(d, b.label)
    }.sortBy(d => (d.bundle, d.id, d.slug))

  def decisionsIn(kb: Kb, bundle: String): Seq[Decision] =
    decisions(kb).filter(_.bundle == bundle)

  def find(kb: Kb, id: String): Option[Decision] =
    val wanted = id.trim
    val padded = wanted.takeWhile(_.isDigit).toIntOption.map(i => f"$i%04d")
    decisions(kb).find(d => d.slug == wanted || d.id == wanted || padded.contains(d.id))

  // ---------------------------------------------------------------- checks

  /** Every finding a decision record can produce. Grouped per bundle, because ids are unique within a bundle rather
    * than across the whole knowledge base — two bundles may each number their decisions from 0001.
    */
  def findings(kb: Kb): Seq[Finding] =
    // Mirrored records are upstream's, and so is their schema: an ADR imported from finos/morphir carries its status
    // and date in the body under upstream's own conventions, and its filename is `ADR-0001-…` rather than `0001-…`.
    // Demanding this register's shape of it would report four errors per file that nobody here can fix. They stay
    // listable — `decisions` still finds them — because they are decision records; they are just not ours to check.
    val all = decisions(kb).filterNot(_.doc.vendored)
    all.groupBy(_.bundle).toSeq.sortBy(_._1).flatMap { (_, inBundle) =>
      val byId = inBundle.filter(_.id.nonEmpty).groupBy(_.id)
      inBundle.flatMap(one(kb, _, byId)) ++ duplicates(kb, byId)
    }

  private def duplicates(kb: Kb, byId: Map[String, Seq[Decision]]): Seq[Finding] =
    byId.filter(_._2.size > 1).toSeq.sortBy(_._1).flatMap { (id, ds) =>
      ds.map(d =>
        err(kb, d, "decision-duplicate-id", s"decision id `$id` is used by ${ds.size} records in ${d.bundle}",
          Some("ids come from the filename prefix; renumber one of them"))
      )
    }

  private def one(kb: Kb, d: Decision, byId: Map[String, Seq[Decision]]): Seq[Finding] =
    val idErrors =
      if d.id.nonEmpty then Nil
      else Seq(err(kb, d, "decision-no-id", "decision record filename does not start with a numeric id",
        Some("name it NNNN-slug.md, e.g. 0004-bridge-nothing-between-zio-and-kyo.md")))

    val stateErrors = d.state match
      case None =>
        Seq(err(kb, d, "decision-state-unknown",
          d.doc.fm.str("state").fold("decision record has no `state`")(s => s"`state: $s` is not a known state"),
          Some(s"one of ${DecisionState.names}")))
      case Some(_) => Nil

    val decidedWarn = Option.when(d.decided.isEmpty)(
      warn(kb, d, "decision-decided-missing", "decision record has no valid `decided` date (YYYY-MM-DD)",
        Some("a decision without a date cannot be read in sequence with the others"))
    ).toSeq

    val obligations = d.state.toSeq.flatMap {
      case DecisionState.Superseded =>
        d.supersededBy match
          case None =>
            Seq(err(kb, d, "decision-superseded-no-successor", "Superseded decision has no `superseded_by`",
              Some("a superseded record must say what replaced it, or a reader has nowhere to go")))
          case Some(succ) if !byId.contains(succ) =>
            Seq(err(kb, d, "decision-superseded-unknown", s"`superseded_by: $succ` names no decision record in ${d.bundle}"))
          case _ => Nil
      case DecisionState.Withdrawn =>
        Option.when(d.reason.forall(_.trim.isEmpty))(
          err(kb, d, "decision-withdrawn-no-reason", "Withdrawn decision has no `reason`",
            Some("a withdrawal without a reason is worthless six months on"))
        ).toSeq
      case _ => Nil
    }

    // `supersedes` must name real records, and the record it names should point back. One-way supersession is how a
    // chain silently breaks: the old record still reads as current to anyone who lands on it directly.
    val forward = d.supersedes.flatMap { target =>
      byId.get(target) match
        case None =>
          Seq(err(kb, d, "decision-supersedes-unknown", s"`supersedes: $target` names no decision record in ${d.bundle}"))
        case Some(targets) =>
          targets.filterNot(_.supersededBy.contains(d.id)).map { t =>
            warn(kb, d, "decision-supersede-not-mutual",
              s"this record supersedes ${t.id} but ${t.id} does not name it in `superseded_by`",
              Some(s"add `superseded_by: \"${d.id}\"` and `state: Superseded` to ${t.slug}.md"))
          }
    }

    idErrors ++ stateErrors ++ decidedWarn ++ obligations ++ forward

  private def err(kb: Kb, d: Decision, check: String, msg: String, hint: Option[String] = None): Finding =
    Finding(Severity.Error, check, kb.rel(d.doc.file), Some(1), msg, hint)

  private def warn(kb: Kb, d: Decision, check: String, msg: String, hint: Option[String] = None): Finding =
    Finding(Severity.Warn, check, kb.rel(d.doc.file), Some(1), msg, hint)

  // ---------------------------------------------------------------- rendering

  def renderList(ds: Seq[Decision], json: Boolean): String =
    if json then
      ujson.write(ujson.Arr.from(ds.map(toJson)), indent = 2) + "\n"
    else if ds.isEmpty then "no decision records\n"
    else
      val sb = StringBuilder()
      DecisionState.displayOrder.foreach { st =>
        val group = ds.filter(_.state.contains(st))
        if group.nonEmpty then
          sb ++= s"\n${st.toString} (${group.size})\n"
          group.foreach { d =>
            sb ++= f"  ${d.id}%-6s ${d.title}\n"
            d.description.foreach(x => sb ++= s"         $x\n")
            d.supersededBy.foreach(x => sb ++= s"         superseded by $x\n")
          }
      }
      val unstated = ds.filter(_.state.isEmpty)
      if unstated.nonEmpty then
        sb ++= s"\nNo state (${unstated.size})\n"
        unstated.foreach(d => sb ++= f"  ${d.id}%-6s ${d.title}\n")
      sb ++= s"\n${ds.size} decision record(s)\n"
      sb.toString

  def renderShow(d: Decision, body: Boolean, json: Boolean): String =
    if json then ujson.write(toJson(d), indent = 2) + "\n"
    else
      val sb = StringBuilder()
      sb ++= s"${d.id} — ${d.title}\n"
      d.description.foreach(x => sb ++= s"$x\n")
      sb ++= s"\nbundle:     ${d.bundle}\n"
      sb ++= s"state:      ${d.state.map(_.toString).getOrElse("(none)")}\n"
      d.decided.foreach(x => sb ++= s"decided:    $x\n")
      if d.supersedes.nonEmpty then sb ++= s"supersedes: ${d.supersedes.mkString(", ")}\n"
      d.supersededBy.foreach(x => sb ++= s"superseded_by: $x\n")
      d.reason.foreach(x => sb ++= s"reason:     $x\n")
      if d.tags.nonEmpty then sb ++= s"tags:       ${d.tags.mkString(", ")}\n"
      sb ++= s"path:       ${d.bundle}:${d.doc.bundlePath}\n"
      if body then sb ++= s"\n${d.doc.body}\n"
      sb.toString

  private def toJson(d: Decision): ujson.Obj =
    ujson.Obj(
      "id" -> d.id,
      "slug" -> d.slug,
      "title" -> d.title,
      "description" -> d.description.map(ujson.Str(_)).getOrElse(ujson.Null),
      "bundle" -> d.bundle,
      "path" -> d.doc.bundlePath,
      "state" -> d.state.map(s => ujson.Str(s.toString)).getOrElse(ujson.Null),
      "decided" -> d.decided.map(x => ujson.Str(x.toString)).getOrElse(ujson.Null),
      "supersedes" -> ujson.Arr.from(d.supersedes.map(ujson.Str(_))),
      "superseded_by" -> d.supersededBy.map(ujson.Str(_)).getOrElse(ujson.Null),
      "reason" -> d.reason.map(ujson.Str(_)).getOrElse(ujson.Null),
      "tags" -> ujson.Arr.from(d.tags.map(ujson.Str(_)))
    )
