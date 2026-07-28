//| scalaVersion: 3.8.4
//| moduleDeps: [KbModel.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5
//| - org.yaml:snakeyaml:2.6
//| - org.commonmark:commonmark:0.29.0

/** Loading and parsing: filesystem to [[Kb]].
  *
  * Frontmatter is parsed with SnakeYAML (full YAML, so nested `sources` entries survive), and the body with
  * commonmark-java (so links come from a real parser rather than a regex that would trip over code fences). File
  * access goes through `kyo.Path`, so everything below the pure-parsing layer is a `Sync` effect.
  */

import kyo.*
import scala.jdk.CollectionConverters.*
import org.commonmark.node.{AbstractVisitor, Link, Node, Text}
import org.commonmark.parser.{IncludeSourceSpans, Parser}
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
import org.yaml.snakeyaml.constructor.SafeConstructor

object KbStore:

  private val parser: Parser =
    Parser.builder().includeSourceSpans(IncludeSourceSpans.BLOCKS_AND_INLINES).build()

  private def newYaml(): Yaml =
    val opts = LoaderOptions()
    opts.setAllowDuplicateKeys(false)
    Yaml(SafeConstructor(opts))

  // ------------------------------------------------------------ pure parsing

  /** Splits a leading `---` fenced YAML block off the front of a document.
    *
    * Returns `(rawFrontmatterOrNone, body)`. A document with no leading fence yields `(None, wholeText)`.
    */
  def splitFrontmatter(text: String): (Option[String], String) =
    val normalized = text.replace("\r\n", "\n")
    if !normalized.startsWith("---\n") then (None, normalized)
    else
      val rest = normalized.drop(4)
      findClosingFence(rest) match
        case Some(i) => (Some(rest.take(i)), rest.drop(i).dropWhile(_ != '\n').drop(1))
        case None => (None, normalized)

  private def findClosingFence(s: String): Option[Int] =
    var idx = 0
    var result: Option[Int] = None
    var done = false
    while !done && !result.isDefined do
      val lineEnd = s.indexOf('\n', idx) match
        case -1 => s.length
        case n => n
      if s.substring(idx, lineEnd).trim == "---" then result = Some(idx)
      else if lineEnd >= s.length then done = true
      else idx = lineEnd + 1
    result

  /** Parses a YAML frontmatter block. Returns Left with the parser message when the YAML is malformed. */
  def parseFrontmatter(raw: String): Either[String, Frontmatter] =
    try
      newYaml().load[Object](raw) match
        case null => Right(Frontmatter.Empty)
        case m: java.util.Map[?, ?] =>
          Right(Frontmatter(m.asScala.toMap.map((k, v) => k.toString -> v.asInstanceOf[Any])))
        case other => Left(s"frontmatter is ${other.getClass.getSimpleName}, expected a mapping")
    catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString).linesIterator.next())

  private def lineOf(node: Node): Int =
    var cur: Node | Null = node
    var line = 0
    while cur != null && line == 0 do
      val spans = cur.getSourceSpans
      if spans != null && !spans.isEmpty then line = spans.get(0).getLineIndex + 1
      cur = cur.getParent
    line

  private def linkText(link: Link): String =
    val sb = StringBuilder()
    link.accept(new AbstractVisitor:
      override def visit(t: Text): Unit = sb.append(t.getLiteral)
    )
    sb.toString

  /** Extracts every inline link from a markdown body, with 1-based line numbers.
    *
    * `frontmatterLines` shifts line numbers so they refer to the original file rather than the stripped body.
    */
  def extractLinks(body: String, frontmatterLines: Int): Seq[LinkRef] =
    val doc = parser.parse(body)
    val out = collection.mutable.ArrayBuffer.empty[LinkRef]
    doc.accept(new AbstractVisitor:
      override def visit(l: Link): Unit =
        out += LinkRef(linkText(l), l.getDestination, lineOf(l) + frontmatterLines)
        visitChildren(l)
    )
    out.toSeq

  def kindOf(rel: Seq[String]): DocKind =
    rel.lastOption match
      case Some("log.md") => DocKind.Log
      case Some("index.md") => if rel.length == 1 then DocKind.RootIndex else DocKind.SubIndex
      case _ => DocKind.Concept

  /** Builds a Doc from already-read text — the pure core of `loadDoc`. */
  def parseDoc(file: Path, bundleRoot: Path, text: String): Doc =
    val (rawFm, body) = splitFrontmatter(text)
    val parsed = rawFm.map(parseFrontmatter)
    val fmLines = rawFm.map(_.count(_ == '\n') + 2).getOrElse(0)
    val rel = KbPath.segmentsUnder(file, bundleRoot).getOrElse(file.parts.lastOption.toSeq)
    Doc(
      file = file,
      bundleRoot = bundleRoot,
      rel = rel,
      kind = kindOf(rel),
      frontmatter = parsed.flatMap(_.toOption),
      hasFrontmatterBlock = rawFm.isDefined,
      frontmatterError = parsed.collect { case Left(e) => e },
      body = body,
      links = extractLinks(body, fmLines)
    )

  // ------------------------------------------------------------------- I/O

  def isMarkdown(p: Path): Boolean = p.parts.lastOption.exists(_.endsWith(".md"))

  def loadDoc(file: Path, bundleRoot: Path): Doc < (Sync & Abort[Throwable]) =
    file.read.map(parseDoc(file, bundleRoot, _))

  private def isBundleRoot(dir: Path): Boolean < (Sync & Abort[Throwable]) =
    val idx = dir / "index.md"
    idx.exists.map {
      case false => false
      case true =>
        idx.read.map { text =>
          splitFrontmatter(text)._1.flatMap(parseFrontmatter(_).toOption).exists(_.has("okf_version"))
        }
    }

  /** Finds bundle roots under `start`, not descending into a bundle once one is found. */
  def findBundleRoots(start: Path): Chunk[Path] < (Sync & Abort[Throwable]) =
    start.exists.map {
      case false => Chunk.empty[Path]
      case true =>
        isBundleRoot(start).map {
          case true => Chunk(start)
          case false =>
            start.list.map { entries =>
              Kyo.foreach(entries) { e =>
                e.isDirectory.map {
                  case true => findBundleRoots(e)
                  case false => Chunk.empty[Path]
                }
              }.map(_.flattenChunk)
            }
        }
    }

  /** Every markdown file at or below `dir`.
    *
    * Recurses with `list` rather than `walk`: `walk` is a stream and drags `Async` and `Scope` into every caller's
    * effect row, which buys nothing for directory trees this small.
    */
  def markdownUnder(dir: Path): Chunk[Path] < (Sync & Abort[Throwable]) =
    dir.list.map { entries =>
      Kyo.foreach(entries) { e =>
        e.isDirectory.map {
          case true => markdownUnder(e)
          case false => if isMarkdown(e) then Chunk(e) else Chunk.empty[Path]
        }
      }.map(_.flattenChunk)
    }

  def loadBundle(root: Path, kbRoot: Path): Bundle < (Sync & Abort[Throwable]) =
    for
      files <- markdownUnder(root)
      docs <- Kyo.foreach(files)(loadDoc(_, root))
    yield
      val segs = KbPath.segmentsUnder(root, kbRoot / "bundles").getOrElse(root.parts.lastOption.toSeq)
      val sorted = docs.sortBy(_.rel.mkString("/"))
      Bundle(
        root = root,
        name = root.parts.lastOption.getOrElse("?"),
        group = Option.when(segs.length > 1)(segs.dropRight(1).mkString("/")),
        index = sorted.find(_.kind == DocKind.RootIndex).getOrElse(
          sys.error(s"${KbPath.render(root)} has no root index.md")
        ),
        log = sorted.find(d => d.kind == DocKind.Log && d.rel.length == 1),
        subIndexes = sorted.filter(_.kind == DocKind.SubIndex),
        concepts = sorted.filter(_.isConcept)
      )

  /** Loads the whole knowledge base rooted at `kbRoot` — the directory holding `bundles/`. */
  def load(kbRoot: Path): Kb < (Sync & Abort[Throwable]) =
    val bundlesDir = kbRoot / "bundles"
    for
      roots <- findBundleRoots(bundlesDir)
      bundles <- Kyo.foreach(roots.sortBy(_.parts.mkString("/")))(loadBundle(_, kbRoot))
      present <- bundlesDir.exists
      allMd <- if present then markdownUnder(bundlesDir) else (Chunk.empty[Path]: Chunk[Path] < (Sync & Abort[Throwable]))
    yield
      val claimed = bundles.map(_.root)
      // A README.md in a grouping directory is expected and correct — it is how a group announces itself without
      // being mistaken for a bundle root.
      val strays = allMd
        .filterNot(p => claimed.exists(r => KbPath.isUnder(p, r)))
        .filterNot(_.parts.lastOption.contains("README.md"))
      Kb(kbRoot, bundles.toSeq, strays.toSeq)

  /** Resolves a link destination to a path, or None when it is external or an anchor. */
  def resolveLink(doc: Doc, link: LinkRef): Option[Path] =
    if link.isExternal || link.isAnchorOnly || link.dest.isEmpty then None
    else
      val dest = link.dest.takeWhile(_ != '#')
      if dest.isEmpty then None
      else if dest.startsWith("/") then Some(doc.bundleRoot / dest.stripPrefix("/"))
      else
        // Relative to the containing directory, resolving any `..` segments.
        val start = doc.file.parts.dropRight(1) ++ dest.split('/')
        val resolved = start.foldLeft(Vector.empty[String]) { (acc, seg) =>
          seg match
            case "." | "" => acc
            case ".." => acc.dropRight(1)
            case s => acc :+ s
        }
        Some(Path(resolved*))
