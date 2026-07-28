//| scalaVersion: 3.8.4
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5
//| - org.yaml:snakeyaml:2.6

/** Domain model for an Open Knowledge Format knowledge base.
  *
  * Pure data and pure functions only — see KbStore.scala for anything that touches the filesystem. Paths are
  * `kyo.Path`, which is a value over its segments and so compares and relativizes predictably.
  */

import kyo.*
import scala.jdk.CollectionConverters.*

/** Segment arithmetic over `kyo.Path`.
  *
  * `kyo.Path` is a list of segments, so containment and relativization are plain sequence operations. Call `realPath`
  * before comparing paths that may reach the same file through different symlinks.
  */
object KbPath:
  def isUnder(child: Path, base: Path): Boolean =
    child.parts.startsWith(base.parts)

  /** Segments of `child` below `base`, or None when `child` is not under `base`. */
  def segmentsUnder(child: Path, base: Path): Option[Seq[String]] =
    Option.when(isUnder(child, base))(child.parts.drop(base.parts.size).toSeq)

  def render(p: Path): String = p.parts.filter(_.nonEmpty).mkString("/", "/", "")

/** What role a markdown file plays inside a bundle. Only `index.md` and `log.md` are reserved by OKF; every other
  * `.md` file is a concept document.
  */
enum DocKind:
  case RootIndex, SubIndex, Log, Concept

/** A link found in a document body. */
case class LinkRef(text: String, dest: String, line: Int):
  def isExternal: Boolean = dest.startsWith("http://") || dest.startsWith("https://") || dest.startsWith("mailto:")
  def isAnchorOnly: Boolean = dest.startsWith("#")
  def isBundleRelative: Boolean = dest.startsWith("/")

/** A provenance entry from the `sources` frontmatter family. */
case class SourceRef(id: Option[String], resource: String, title: Option[String])

/** Parsed YAML frontmatter, kept as raw values with typed accessors over the top.
  *
  * Accessors are deliberately permissive: a missing or wrongly-typed field yields None/Nil rather than throwing, so
  * that `kb check` reports every problem in one pass instead of dying on the first.
  */
case class Frontmatter(values: Map[String, Any]):
  def has(key: String): Boolean = values.contains(key)

  def str(key: String): Option[String] = values.get(key).collect {
    case s: String => s
    case i: Integer => i.toString
    case d: java.lang.Double => d.toString
  }

  def strList(key: String): List[String] = values.get(key) match
    case Some(l: java.util.List[?]) => l.asScala.toList.collect { case s: String => s }
    case Some(s: String) => List(s)
    case _ => Nil

  private def mapsAt(key: String): List[Map[String, Any]] = values.get(key) match
    case Some(l: java.util.List[?]) =>
      l.asScala.toList.collect { case m: java.util.Map[?, ?] => m.asScala.toMap.map((k, v) => k.toString -> v) }
    case Some(m: java.util.Map[?, ?]) =>
      List(m.asScala.toMap.map((k, v) => k.toString -> v))
    case _ => Nil

  def `type`: Option[String] = str("type")
  def title: Option[String] = str("title")
  def description: Option[String] = str("description")
  def resource: Option[String] = str("resource")
  def status: Option[String] = str("status")
  def staleAfter: Option[String] = str("stale_after")
  def okfVersion: Option[String] = str("okf_version")
  def tags: List[String] = strList("tags")

  def sources: List[SourceRef] = mapsAt("sources").flatMap { m =>
    m.get("resource").collect { case r: String => r }.map { r =>
      SourceRef(
        id = m.get("id").collect { case s: String => s },
        resource = r,
        title = m.get("title").collect { case s: String => s }
      )
    }
  }

object Frontmatter:
  val Empty: Frontmatter = Frontmatter(Map.empty)

  /** Frontmatter keys OKF v0.2 defines. Anything else is producer-specific and merely reported, never rejected. */
  val Known: Set[String] = Set(
    "type", "title", "description", "resource", "tags",
    "sources", "generated", "verified", "status", "stale_after",
    "runtime", "parameters", "computation", "executor", "attester",
    "okf_version"
  )

  val Statuses: Set[String] = Set("draft", "stable", "deprecated")

/** A single markdown file within a bundle. `rel` holds its path segments below the bundle root. */
case class Doc(
    file: Path,
    bundleRoot: Path,
    rel: Seq[String],
    kind: DocKind,
    frontmatter: Option[Frontmatter],
    /** True when a leading `---` block was present, whether or not it parsed. */
    hasFrontmatterBlock: Boolean,
    /** Set when a frontmatter block was present but did not parse as YAML. */
    frontmatterError: Option[String],
    body: String,
    links: Seq[LinkRef]
):
  /** Bundle-relative path in OKF link form, e.g. `/design/annotations.md`. */
  def bundlePath: String = rel.mkString("/", "/", "").stripSuffix("/")
  def name: String = rel.lastOption.getOrElse("")
  def isConcept: Boolean = kind == DocKind.Concept
  def fm: Frontmatter = frontmatter.getOrElse(Frontmatter.Empty)
  def displayTitle: String = fm.title.getOrElse(name.stripSuffix(".md"))

/** An OKF bundle: a directory whose root `index.md` carries `okf_version`. */
case class Bundle(
    root: Path,
    name: String,
    group: Option[String],
    index: Doc,
    log: Option[Doc],
    subIndexes: Seq[Doc],
    concepts: Seq[Doc]
):
  def label: String = group.map(g => s"$g/$name").getOrElse(name)
  def allDocs: Seq[Doc] = Seq(index) ++ log ++ subIndexes ++ concepts
  def allIndexes: Seq[Doc] = index +: subIndexes
  def okfVersion: Option[String] = index.fm.okfVersion
  def conceptAt(bundlePath: String): Option[Doc] =
    concepts.find(_.bundlePath == bundlePath)

/** A loaded knowledge base. */
case class Kb(root: Path, bundles: Seq[Bundle], strays: Seq[Path]):
  def bundle(label: String): Option[Bundle] =
    bundles.find(b => b.label == label || b.name == label)
  def concepts: Seq[(Bundle, Doc)] = bundles.flatMap(b => b.concepts.map(b -> _))
  def rel(p: Path): String =
    KbPath.segmentsUnder(p, root).map(s => (root.parts.lastOption.toSeq ++ s).mkString("/")).getOrElse(KbPath.render(p))
