//| scalaVersion: 3.8.4
//| moduleDeps: [KbStore.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Conformance and drift checks over a knowledge base.
  *
  * Two families:
  *   - structural — does the KB obey OKF and the conventions in kb/AGENTS.md?
  *   - provenance — do the commit-pinned `sources` still line up with the reference checkouts under `.refs/`?
  *
  * Everything is offline. Provenance runs `git` against local checkouts; it never reaches the network.
  */

import kyo.*
import java.time.LocalDate

enum Severity:
  case Error, Warn, Info
  def label: String = this match
    case Error => "error"
    case Warn => "warn"
    case Info => "info"

case class Finding(
    severity: Severity,
    check: String,
    path: String,
    line: Option[Int],
    message: String,
    hint: Option[String] = None
):
  def location: String = line.map(l => s"$path:$l").getOrElse(path)

object KbCheck:

  private val IndexEntry = raw"^\s*[*-]\s+\[([^\]]*)\]\(([^)]+)\)\s*(?:-\s*(.*))?$$".r
  private val GitHubUrl = raw"https://github\.com/([^/]+)/([^/]+)/(?:blob|tree)/([0-9a-f]{7,40})/(.*)".r

  /** Runs every check. `today` is injected so results are reproducible. */
  def run(kb: Kb, refsRoot: Option[Path], today: LocalDate): Seq[Finding] < (Async & Abort[Throwable]) =
    for
      structural <- Kyo.foreach(kb.bundles)(bundleFindings(kb, _, today))
      provenance <- refsRoot match
        case None => (Chunk.empty[Chunk[Finding]]: Chunk[Chunk[Finding]] < (Async & Abort[Throwable]))
        case Some(r) => Kyo.foreach(kb.concepts)((_, d) => sourceFindings(kb, d, r))
    yield
      val strays = kb.strays.map { p =>
        Finding(
          Severity.Error,
          "stray-markdown",
          kb.rel(p),
          None,
          "markdown file under bundles/ that belongs to no bundle",
          Some("a bundle root is a directory whose index.md carries okf_version; grouping directories use README.md")
        )
      }
      (structural.flatten.toSeq ++ strays ++ provenance.flatten.toSeq)
        .sortBy(f => (f.severity.ordinal, f.path, f.line.getOrElse(0)))

  private def bundleFindings(kb: Kb, b: Bundle, today: LocalDate): Chunk[Finding] < (Sync & Abort[Throwable]) =
    Kyo.foreach(b.allDocs)(docFindings(kb, b, _, today)).map { perDoc =>
      Chunk.from(perDoc.flatten) ++
        Chunk.from(indexCoverage(kb, b) ++ indexDescriptions(kb, b) ++ duplicateTitles(kb, b) ++ readmeInBundle(kb, b))
    }

  private def docFindings(kb: Kb, b: Bundle, d: Doc, today: LocalDate): Chunk[Finding] < (Sync & Abort[Throwable]) =
    linkFindings(kb, d).map(links => Chunk.from(pureDocFindings(kb, d, today)) ++ links)

  /** Everything about a document that can be decided without touching the filesystem. */
  def pureDocFindings(kb: Kb, d: Doc, today: LocalDate): Seq[Finding] =
    val where = kb.rel(d.file)

    val fmErrors = d.frontmatterError.toSeq.map { msg =>
      Finding(Severity.Error, "frontmatter-invalid", where, Some(1), s"YAML did not parse: $msg")
    }

    val kindErrors = d.kind match
      case DocKind.SubIndex if d.hasFrontmatterBlock =>
        Seq(Finding(
          Severity.Error,
          "subindex-has-frontmatter",
          where,
          Some(1),
          "a non-root index.md must have no frontmatter",
          Some("only the bundle-root index.md carries frontmatter, and there only okf_version and friends")
        ))
      case DocKind.Concept if !d.hasFrontmatterBlock =>
        Seq(Finding(
          Severity.Error,
          "concept-no-frontmatter",
          where,
          Some(1),
          "concept document has no frontmatter block",
          Some("every concept needs at least `type:`")
        ))
      case _ => Nil

    val conceptErrors =
      if !d.isConcept || !d.hasFrontmatterBlock then Nil
      else
        val fm = d.fm
        Seq(
          Option.when(fm.`type`.isEmpty)(
            Finding(Severity.Error, "concept-missing-type", where, Some(1), "concept has no `type` — the one universally required OKF field")
          ),
          Option.when(fm.title.isEmpty)(Finding(Severity.Warn, "concept-missing-title", where, Some(1), "concept has no `title`")),
          Option.when(fm.description.isEmpty)(
            Finding(Severity.Warn, "concept-missing-description", where, Some(1), "concept has no `description` — indexes and search snippets read it")
          ),
          fm.status.filterNot(Frontmatter.Statuses.contains).map(s =>
            Finding(Severity.Warn, "status-unknown", where, Some(1), s"status `$s` is not one of ${Frontmatter.Statuses.toSeq.sorted.mkString(", ")}")
          ),
          fm.staleAfter.flatMap(parseDate).filter(_.isBefore(today)).map(dt =>
            Finding(Severity.Warn, "stale-after-passed", where, Some(1), s"stale_after $dt has passed — content is due for review")
          )
        ).flatten ++ unknownKeys(where, d)

    fmErrors ++ kindErrors ++ conceptErrors

  private def unknownKeys(where: String, d: Doc): Seq[Finding] =
    d.fm.values.keys.filterNot(Frontmatter.Known.contains).toSeq.sorted.map { k =>
      Finding(Severity.Info, "frontmatter-unknown-key", where, Some(1), s"frontmatter key `$k` is not defined by OKF v0.2")
    }

  private def parseDate(s: String): Option[LocalDate] =
    try Some(LocalDate.parse(s.trim)) catch case _: Exception => None

  private def linkFindings(kb: Kb, d: Doc): Chunk[Finding] < (Sync & Abort[Throwable]) =
    val resolvable = d.links.flatMap(l => KbStore.resolveLink(d, l).map(l -> _))
    Kyo.foreach(Chunk.from(resolvable)) { (link, target) =>
      target.exists.map {
        case true => Chunk.empty[Finding]
        case false =>
          Chunk(Finding(
            Severity.Error,
            "link-broken",
            kb.rel(d.file),
            Some(link.line),
            s"link target does not exist: ${link.dest}",
            Option.when(link.isBundleRelative)("bundle-relative paths start at the bundle root, not the kb root")
          ))
      }
    }.map(_.flattenChunk)

  /** Every concept should be reachable from an index in its own bundle. */
  private def indexCoverage(kb: Kb, b: Bundle): Seq[Finding] =
    val linked: Set[String] = b.allIndexes.flatMap(_.links.filter(_.isBundleRelative).map(_.dest.takeWhile(_ != '#'))).toSet
    b.concepts.filterNot(c => linked.contains(c.bundlePath)).map { c =>
      Finding(
        Severity.Warn,
        "concept-not-indexed",
        kb.rel(c.file),
        None,
        s"concept is not linked from any index in ${b.label}",
        Some(s"add `* [${c.displayTitle}](${c.bundlePath}) - ${c.fm.description.getOrElse("…")}` to an index")
      )
    }

  /** Index bullets mirror the target's `description`; drift between them is a real inconsistency. */
  private def indexDescriptions(kb: Kb, b: Bundle): Seq[Finding] =
    b.allIndexes.flatMap { idx =>
      idx.body.linesIterator.zipWithIndex.flatMap { (line, i) =>
        IndexEntry.findFirstMatchIn(line).toSeq.flatMap { m =>
          val dest = m.group(2)
          val text = Option(m.group(3)).map(_.trim).filter(_.nonEmpty)
          if !dest.startsWith("/") then Nil
          else
            b.conceptAt(dest.takeWhile(_ != '#')).toSeq.flatMap { target =>
              (text, target.fm.description.map(_.trim)) match
                case (Some(t), Some(dsc)) if normalize(t) != normalize(dsc) =>
                  Seq(Finding(
                    Severity.Warn,
                    "index-description-drift",
                    kb.rel(idx.file),
                    Some(i + 1),
                    s"index entry text differs from the concept's description ($dest)",
                    Some(s"concept says: $dsc")
                  ))
                case _ => Nil
            }
        }
      }.toSeq
    }

  private def normalize(s: String): String =
    s.trim.stripSuffix(".").replaceAll("\\s+", " ").toLowerCase

  private def duplicateTitles(kb: Kb, b: Bundle): Seq[Finding] =
    b.concepts.groupBy(_.displayTitle).filter(_._2.size > 1).toSeq.sortBy(_._1).flatMap { (title, docs) =>
      docs.map(d =>
        Finding(Severity.Warn, "duplicate-title", kb.rel(d.file), Some(1), s"title `$title` is used by ${docs.size} concepts in ${b.label}")
      )
    }

  private def readmeInBundle(kb: Kb, b: Bundle): Seq[Finding] =
    b.concepts.filter(_.name.equalsIgnoreCase("README.md")).map { d =>
      Finding(
        Severity.Error,
        "readme-in-bundle",
        kb.rel(d.file),
        None,
        "README.md inside a bundle is parsed as a concept document",
        Some("put bundle orientation in index.md; README.md belongs to grouping directories only")
      )
    }

  // ---------------------------------------------------------------- provenance

  /** Current HEAD of a local checkout, or None when it is not a git repository. */
  def gitHead(repo: Path): Option[String] < (Async & Abort[Throwable]) =
    Abort.run[Throwable](
      Command("git", "-C", KbPath.render(repo), "rev-parse", "HEAD").textWithExitCode
    ).map {
      // Validate by shape rather than by exit code: `kyo.Process.ExitCode` is opaque, and a successful
      // `git rev-parse HEAD` is exactly one 40-character hex SHA. Anything else means "not a git checkout".
      case Result.Success((out, _)) if out.trim.matches("[0-9a-f]{40}") => Some(out.trim)
      case _ => None
    }

  /** Compares each commit-pinned GitHub source against the matching checkout under `.refs/<org>/<repo>`. */
  private def sourceFindings(kb: Kb, d: Doc, refsRoot: Path): Chunk[Finding] < (Async & Abort[Throwable]) =
    val where = kb.rel(d.file)
    val refsName = refsRoot.parts.lastOption.getOrElse(".refs")
    Kyo.foreach(Chunk.from(d.fm.sources)) { src =>
      src.resource match
        case GitHubUrl(org, repo, sha, path) =>
          val checkout = refsRoot / org / repo
          checkout.exists.map {
            case false =>
              Chunk(Finding(
                Severity.Info,
                "source-ref-missing",
                where,
                None,
                s"no reference checkout for $org/$repo under $refsName/",
                Some("add one so provenance can be verified: /squire reference repo add")
              ))
            case true =>
              val cleanPath = path.takeWhile(_ != '#')
              for
                head <- gitHead(checkout)
                pathOk <- if cleanPath.isEmpty then (true: Boolean < (Async & Abort[Throwable])) else (checkout / cleanPath).exists
              yield
                val drift = head.filter(h => !h.startsWith(sha) && !sha.startsWith(h.take(sha.length))).toSeq.map { h =>
                  Finding(
                    Severity.Warn,
                    "source-commit-drift",
                    where,
                    None,
                    s"source pinned at ${sha.take(8)} but $refsName/$org/$repo is at ${h.take(8)}",
                    Some("re-read the source and update the concept, or leave the pin and accept it is historical")
                  )
                }
                val missing =
                  if pathOk then Nil
                  else
                    Seq(Finding(
                      Severity.Warn,
                      "source-path-missing",
                      where,
                      None,
                      s"source path no longer present at the checkout's HEAD: $cleanPath",
                      Some("the file moved or was deleted upstream; the pinned URL still resolves on GitHub")
                    ))
                Chunk.from(drift ++ missing)
          }
        case _ => (Chunk.empty[Finding]: Chunk[Finding] < (Async & Abort[Throwable]))
    }.map(_.flattenChunk)

  // ---------------------------------------------------------------- rendering

  def renderText(findings: Seq[Finding], verbose: Boolean): String =
    val shown = if verbose then findings else findings.filter(_.severity != Severity.Info)
    val sb = StringBuilder()
    shown.foreach { f =>
      sb ++= f"${f.severity.label}%-5s  ${f.check}%-26s  ${f.location}\n"
      sb ++= s"       ${f.message}\n"
      f.hint.foreach(h => sb ++= s"       hint: $h\n")
    }
    val e = findings.count(_.severity == Severity.Error)
    val w = findings.count(_.severity == Severity.Warn)
    val i = findings.count(_.severity == Severity.Info)
    if shown.isEmpty then sb ++= "no findings\n"
    sb ++= s"\n$e error(s), $w warning(s), $i info"
    if !verbose && i > 0 then sb ++= " (hidden; pass --verbose)"
    sb ++= "\n"
    sb.toString

  def renderJson(findings: Seq[Finding]): String =
    ujson.write(
      ujson.Obj(
        "errors" -> findings.count(_.severity == Severity.Error),
        "warnings" -> findings.count(_.severity == Severity.Warn),
        "infos" -> findings.count(_.severity == Severity.Info),
        "findings" -> ujson.Arr.from(findings.map { f =>
          ujson.Obj(
            "severity" -> f.severity.label,
            "check" -> f.check,
            "path" -> f.path,
            "line" -> f.line.map(l => ujson.Num(l.toDouble)).getOrElse(ujson.Null),
            "message" -> f.message,
            "hint" -> f.hint.map(ujson.Str(_)).getOrElse(ujson.Null)
          )
        })
      ),
      indent = 2
    ) + "\n"
