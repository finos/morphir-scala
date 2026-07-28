//| scalaVersion: 3.8.4
//| moduleDeps: [KbStore.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Creating bundles and concepts, and keeping indexes and logs in step.
  *
  * Scaffolding writes the frontmatter skeleton and wires the concept into its index and log. It deliberately does not
  * write prose — that is the agent's job, and a stub that reads as finished is worse than an obvious stub.
  */

import kyo.*
import java.time.LocalDate

case class ScaffoldResult(created: Seq[Path], updated: Seq[Path], notes: Seq[String])

object KbScaffold:

  /** kebab-cases a free-form name into a slug usable as a directory or filename. */
  def slugify(s: String): String =
    s.trim.toLowerCase.replaceAll("[^a-z0-9]+", "-").replaceAll("^-+|-+$", "")

  private def yamlStr(s: String): String =
    val needsQuote = s.exists(c => ":#{}[]&*!|>'\"%@`,".contains(c)) || s.startsWith(" ") || s.endsWith(" ")
    if needsQuote then "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"" else s

  private def descend(base: Path, relPath: String): Path =
    relPath.split('/').filter(_.nonEmpty).foldLeft(base)(_ / _)

  // ------------------------------------------------------------- new bundle

  def newBundle(
      kbRoot: Path,
      name: String,
      group: Option[String],
      title: String,
      description: String,
      okfVersion: String,
      today: LocalDate
  ): Either[String, ScaffoldResult] < (Sync & Abort[Throwable]) =
    val slug = slugify(name)
    val dir = group.map(g => descend(kbRoot / "bundles", g) / slug).getOrElse(kbRoot / "bundles" / slug)
    dir.exists.map {
      case true => Left(s"${KbPath.render(dir)} already exists")
      case false =>
        val index = dir / "index.md"
        val log = dir / "log.md"
        val groupDir = group.map(g => descend(kbRoot / "bundles", g))
        for
          _ <- index.write(indexTemplate(okfVersion, title, description))
          _ <- log.write(logTemplate(today))
          groupReadmeMissing <- groupDir match
            case None => (false: Boolean < (Sync & Abort[Throwable]))
            case Some(g) => (g / "README.md").exists.map(!_)
        yield Right(ScaffoldResult(
          created = Seq(index, log),
          updated = Nil,
          notes =
            Option.when(groupReadmeMissing)(s"grouping directory has no README.md yet: ${groupDir.map(KbPath.render).getOrElse("")}").toSeq ++
              Seq(s"add the bundle to the Bundles table in ${KbPath.render(kbRoot / "README.md")}")
        ))
    }

  private def indexTemplate(okfVersion: String, title: String, description: String): String =
    s"""---
       |okf_version: "$okfVersion"
       |title: ${yamlStr(title)}
       |description: ${yamlStr(description)}
       |---
       |
       |# $title
       |
       |$description
       |
       |## Orientation
       |
       |""".stripMargin

  private def logTemplate(today: LocalDate): String =
    s"""# Log
       |
       |## $today
       |
       |* **Creation**: Bundle created.
       |""".stripMargin

  // ------------------------------------------------------------ add concept

  def addConcept(
      bundle: Bundle,
      relPath: String,
      conceptType: String,
      title: String,
      description: String,
      tags: Seq[String],
      status: Option[String],
      sources: Seq[(Option[String], String, Option[String])],
      section: String,
      generatedBy: Option[String],
      today: LocalDate
  ): Either[String, ScaffoldResult] < (Sync & Abort[Throwable]) =
    val relSegs = (if relPath.endsWith(".md") then relPath else relPath + ".md").split('/').filter(_.nonEmpty).toSeq
    val leaf = relSegs.last
    if leaf == "index.md" || leaf == "log.md" then
      (Left(s"$leaf is a reserved OKF filename"): Either[String, ScaffoldResult] < (Sync & Abort[Throwable]))
    else
      val target = relSegs.foldLeft(bundle.root)(_ / _)
      target.exists.map {
        case true => Left(s"${KbPath.render(target)} already exists")
        case false =>
          val bundlePath = relSegs.mkString("/", "/", "")
          // A concept in a subdirectory belongs in that subdirectory's index when one exists.
          val idxDoc = bundle.subIndexes
            .find(i => relSegs.length > 1 && i.rel.dropRight(1) == relSegs.dropRight(1))
            .getOrElse(bundle.index)
          for
            _ <- target.write(conceptTemplate(conceptType, title, description, tags, status, sources, generatedBy, today))
            _ <- insertIndexEntry(idxDoc.file, section, title, bundlePath, description)
            _ <- bundle.log match
              case None => ((): Unit < (Sync & Abort[Throwable]))
              case Some(l) => appendLogEntry(l.file, today, s"**Creation**: Added [$title]($bundlePath).")
          yield Right(ScaffoldResult(
            created = Seq(target),
            updated = Seq(idxDoc.file) ++ bundle.log.map(_.file),
            notes = Option.when(bundle.log.isEmpty)("bundle has no log.md — creation was not recorded").toSeq
          ))
      }

  private def conceptTemplate(
      conceptType: String,
      title: String,
      description: String,
      tags: Seq[String],
      status: Option[String],
      sources: Seq[(Option[String], String, Option[String])],
      generatedBy: Option[String],
      today: LocalDate
  ): String =
    val sb = StringBuilder()
    sb ++= "---\n"
    sb ++= s"type: ${yamlStr(conceptType)}\n"
    sb ++= s"title: ${yamlStr(title)}\n"
    sb ++= s"description: ${yamlStr(description)}\n"
    if tags.nonEmpty then sb ++= s"tags: [${tags.map(slugify).mkString(", ")}]\n"
    status.foreach(s => sb ++= s"status: $s\n")
    if sources.nonEmpty then
      sb ++= "sources:\n"
      sources.foreach { (id, resource, srcTitle) =>
        sb ++= "  - "
        id.foreach(i => sb ++= s"id: $i\n    ")
        sb ++= s"resource: $resource\n"
        srcTitle.foreach(t => sb ++= s"    title: ${yamlStr(t)}\n")
      }
    generatedBy.foreach { by =>
      sb ++= "generated:\n"
      sb ++= s"  by: $by\n"
      sb ++= s"  at: ${today}T00:00:00Z\n"
    }
    sb ++= "---\n\n"
    sb ++= s"# $title\n\n"
    sb ++= s"$description\n\n"
    sb ++= "<!-- TODO: write the concept body. Delete this comment when done. -->\n"
    sb.toString

  /** Adds `* [title](path) - description` under `## section`, creating the section if it is absent. */
  def insertIndexEntry(index: Path, section: String, title: String, bundlePath: String, description: String): Unit < (Sync & Abort[Throwable]) =
    index.read.map { text =>
      val entry = s"* [$title]($bundlePath) - ${description.trim}"
      val lines = text.replace("\r\n", "\n").linesIterator.toVector
      val headIdx = lines.indexWhere(_.trim.equalsIgnoreCase(s"## $section"))
      val updated =
        if headIdx < 0 then
          lines.reverse.dropWhile(_.trim.isEmpty).reverse ++ Vector("", s"## $section", "", entry)
        else
          // Append after the last bullet of that section, before the next heading.
          val nextHead = lines.indexWhere(_.startsWith("## "), headIdx + 1) match
            case -1 => lines.length
            case n => n
          val body = lines.slice(headIdx + 1, nextHead)
          val lastBullet = body.lastIndexWhere(l => l.trim.startsWith("*") || l.trim.startsWith("-"))
          // With no bullets yet, land at the end of the section so the blank line after the heading survives.
          val at = if lastBullet < 0 then nextHead else headIdx + 1 + lastBullet + 1
          lines.take(at) ++ Vector(entry) ++ lines.drop(at)
      index.write(updated.mkString("\n").stripTrailing() + "\n")
    }

  /** Adds a bullet under `## <today>`, creating that date section at the top (newest first) when absent. */
  def appendLogEntry(log: Path, today: LocalDate, entry: String): Unit < (Sync & Abort[Throwable]) =
    log.read.map { text =>
      val lines = text.replace("\r\n", "\n").linesIterator.toVector
      val bullet = s"* $entry"
      val dateHead = s"## $today"
      val idx = lines.indexWhere(_.trim == dateHead)
      val updated =
        if idx >= 0 then
          val nextHead = lines.indexWhere(_.startsWith("## "), idx + 1) match
            case -1 => lines.length
            case n => n
          val lastBullet = lines.slice(idx + 1, nextHead).lastIndexWhere(_.trim.startsWith("*"))
          val at = if lastBullet < 0 then nextHead else idx + 1 + lastBullet + 1
          lines.take(at) ++ Vector(bullet) ++ lines.drop(at)
        else
          val firstHead = lines.indexWhere(_.startsWith("## "))
          val at = if firstHead < 0 then lines.length else firstHead
          val prefix = lines.take(at).reverse.dropWhile(_.trim.isEmpty).reverse
          prefix ++ Vector("", dateHead, "", bullet, "") ++ lines.drop(at)
      log.write(updated.mkString("\n").stripTrailing() + "\n")
    }
