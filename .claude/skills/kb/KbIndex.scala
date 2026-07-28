//| scalaVersion: 3.8.4
//| moduleDeps: [KbStore.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5
//| - org.xerial:sqlite-jdbc:3.53.2.1

/** A SQLite index over the knowledge base.
  *
  * The index is **derived state**: everything in it is recomputed from the markdown, so it lives under `.dev/` and is
  * never committed. Deleting it costs a rebuild, nothing more.
  *
  * It exists to make three things cheap that are otherwise a full re-parse of every file: full-text search (FTS5),
  * link-graph queries in both directions, and grouping by frontmatter facets — type, tag, status, source repository.
  *
  * JDBC calls sit inside `Sync.defer`; kyo stays at the edge, as it does elsewhere in this skill.
  */

import kyo.*
import java.sql.{Connection, DriverManager, ResultSet}
import java.time.Instant

case class IndexStats(bundles: Int, docs: Int, concepts: Int, links: Int, headings: Int, sources: Int, tags: Int)

object KbIndex:

  /** Bumped whenever the schema changes; a mismatch forces a rebuild rather than querying stale shapes. */
  val SchemaVersion = 2

  /** Must run outside a transaction — SQLite refuses a journal-mode change from within one. */
  private val Pragmas: Seq[String] = Seq("PRAGMA journal_mode=WAL", "PRAGMA synchronous=NORMAL")

  private val Ddl: Seq[String] = Seq(
    """CREATE TABLE meta (
         key   TEXT PRIMARY KEY,
         value TEXT NOT NULL
       )""",
    """CREATE TABLE bundle (
         id          INTEGER PRIMARY KEY,
         label       TEXT NOT NULL UNIQUE,
         name        TEXT NOT NULL,
         grp         TEXT,
         okf_version TEXT,
         title       TEXT,
         description TEXT,
         root_path   TEXT NOT NULL
       )""",
    """CREATE TABLE doc (
         id                INTEGER PRIMARY KEY,
         bundle_id         INTEGER NOT NULL REFERENCES bundle(id),
         bundle_path       TEXT NOT NULL,
         rel_path          TEXT NOT NULL,
         file_path         TEXT NOT NULL,
         kind              TEXT NOT NULL,
         type              TEXT,
         title             TEXT,
         description       TEXT,
         status            TEXT,
         stale_after       TEXT,
         resource          TEXT,
         has_frontmatter   INTEGER NOT NULL,
         frontmatter_error TEXT,
         body_lines        INTEGER NOT NULL,
         body_chars        INTEGER NOT NULL,
         UNIQUE (bundle_id, rel_path)
       )""",
    // Generic key/value over every top-level frontmatter field. Lets other tooling (intent, and whatever comes
    // next) query its own facets without this schema having to learn about them.
    """CREATE TABLE frontmatter (
         doc_id INTEGER NOT NULL REFERENCES doc(id),
         key    TEXT NOT NULL,
         value  TEXT,
         PRIMARY KEY (doc_id, key)
       )""",
    """CREATE TABLE tag (
         doc_id INTEGER NOT NULL REFERENCES doc(id),
         tag    TEXT NOT NULL,
         PRIMARY KEY (doc_id, tag)
       )""",
    """CREATE TABLE source (
         id         INTEGER PRIMARY KEY,
         doc_id     INTEGER NOT NULL REFERENCES doc(id),
         source_id  TEXT,
         resource   TEXT NOT NULL,
         title      TEXT,
         org        TEXT,
         repo       TEXT,
         commit_sha TEXT,
         src_path   TEXT
       )""",
    """CREATE TABLE link (
         id            INTEGER PRIMARY KEY,
         doc_id        INTEGER NOT NULL REFERENCES doc(id),
         dest          TEXT NOT NULL,
         text          TEXT,
         line          INTEGER,
         kind          TEXT NOT NULL,
         target_doc_id INTEGER REFERENCES doc(id)
       )""",
    """CREATE TABLE heading (
         id     INTEGER PRIMARY KEY,
         doc_id INTEGER NOT NULL REFERENCES doc(id),
         level  INTEGER NOT NULL,
         text   TEXT NOT NULL,
         slug   TEXT NOT NULL,
         line   INTEGER NOT NULL
       )""",
    "CREATE INDEX idx_doc_type ON doc(type)",
    "CREATE INDEX idx_doc_status ON doc(status)",
    "CREATE INDEX idx_doc_kind ON doc(kind)",
    "CREATE INDEX idx_tag_tag ON tag(tag)",
    "CREATE INDEX idx_frontmatter_key ON frontmatter(key, value)",
    "CREATE INDEX idx_link_target ON link(target_doc_id)",
    "CREATE INDEX idx_link_kind ON link(kind)",
    "CREATE INDEX idx_source_repo ON source(org, repo)",
    "CREATE INDEX idx_heading_doc ON heading(doc_id)",
    // Detached content: the KB is small and keeping the text here means FTS answers without touching the doc table.
    "CREATE VIRTUAL TABLE doc_fts USING fts5(bundle_path, title, description, body, tokenize='porter unicode61')",
    """CREATE VIEW v_concept AS
         SELECT d.id, b.label AS bundle, d.bundle_path, d.type, d.title, d.description, d.status, d.stale_after
         FROM doc d JOIN bundle b ON b.id = d.bundle_id
         WHERE d.kind = 'Concept'""",
    """CREATE VIEW v_backlink AS
         SELECT l.target_doc_id AS doc_id,
                src.bundle_path AS from_path,
                b.label         AS from_bundle,
                l.line          AS line
         FROM link l
         JOIN doc src ON src.id = l.doc_id
         JOIN bundle b ON b.id = src.bundle_id
         WHERE l.target_doc_id IS NOT NULL""",
    // Intent facets pivoted into columns. Depends only on the generic frontmatter table, so kb knows nothing about
    // intent beyond the fact that `type: Intent` documents exist.
    """CREATE VIEW v_intent AS
         SELECT d.id,
                b.label       AS bundle,
                d.bundle_path,
                d.title,
                d.description,
                MAX(CASE WHEN f.key = 'state'         THEN f.value END) AS state,
                MAX(CASE WHEN f.key = 'kind'          THEN f.value END) AS kind,
                MAX(CASE WHEN f.key = 'breaking'      THEN f.value END) AS breaking,
                MAX(CASE WHEN f.key = 'created'       THEN f.value END) AS created,
                MAX(CASE WHEN f.key = 'state_since'   THEN f.value END) AS state_since,
                MAX(CASE WHEN f.key = 'issue'         THEN f.value END) AS issue,
                MAX(CASE WHEN f.key = 'capability'    THEN f.value END) AS capability,
                MAX(CASE WHEN f.key = 'superseded_by' THEN f.value END) AS superseded_by,
                MAX(CASE WHEN f.key = 'artifacts'     THEN f.value END) AS artifacts
         FROM doc d
         JOIN bundle b ON b.id = d.bundle_id
         LEFT JOIN frontmatter f ON f.doc_id = d.id
         WHERE d.type = 'Intent'
         GROUP BY d.id""",
    """CREATE VIEW v_orphan AS
         SELECT d.id, b.label AS bundle, d.bundle_path, d.title
         FROM doc d
         JOIN bundle b ON b.id = d.bundle_id
         WHERE d.kind = 'Concept'
           AND NOT EXISTS (SELECT 1 FROM link l WHERE l.target_doc_id = d.id)"""
  )

  private val GitHubUrl = raw"https://github\.com/([^/]+)/([^/]+)/(?:blob|tree)/([0-9a-f]{7,40})/(.*)".r
  private val Heading = raw"^(#{1,6})\s+(.*?)\s*#*$$".r

  private def slugify(s: String): String =
    s.trim.toLowerCase.replaceAll("[^a-z0-9]+", "-").replaceAll("^-+|-+$", "")

  private def linkKind(l: LinkRef): String =
    if l.isExternal then "external"
    else if l.isAnchorOnly then "anchor"
    else if l.isBundleRelative then "bundle"
    else "relative"

  /** Headings in a body, with line numbers offset past the frontmatter. Fenced code blocks are skipped so that a
    * `# comment` inside a shell example is not mistaken for a section.
    */
  def headings(body: String): Seq[(Int, String, Int)] =
    var inFence = false
    body.linesIterator.zipWithIndex.flatMap { (line, i) =>
      val t = line.trim
      if t.startsWith("```") || t.startsWith("~~~") then
        inFence = !inFence
        None
      else if inFence then None
      else
        Heading.findFirstMatchIn(line).map(m => (m.group(1).length, m.group(2).trim, i + 1))
    }.toSeq

  // ------------------------------------------------------------------ build

  private def connect(db: Path): Connection =
    Class.forName("org.sqlite.JDBC")
    DriverManager.getConnection(s"jdbc:sqlite:${KbPath.render(db)}")

  /** Rebuilds the index from scratch. The database file is replaced, so a stale schema can never linger. */
  def build(kb: Kb, db: Path): IndexStats < (Sync & Abort[Throwable]) =
    val parent = Path(db.parts.dropRight(1)*)
    // WAL leaves siblings behind; a rebuild that kept them would resurrect pages from the old database.
    val siblings = Seq(db) ++ Seq("-wal", "-shm").map(sfx => Path((db.parts.dropRight(1) :+ (db.parts.last + sfx))*))
    for
      _ <- Sync.defer(java.io.File(KbPath.render(parent)).mkdirs())
      _ <- Kyo.foreachDiscard(Chunk.from(siblings))(p => p.exists.map(if _ then p.remove else ()))
      stats <- Sync.defer(writeAll(kb, db))
    yield stats

  private def writeAll(kb: Kb, db: Path): IndexStats =
    val conn = connect(db)
    try
      val pragmaSt = conn.createStatement()
      Pragmas.foreach(pragmaSt.execute)
      pragmaSt.close()

      conn.setAutoCommit(false)
      val st = conn.createStatement()
      Ddl.foreach(st.execute)
      st.close()

      val metaSt = conn.prepareStatement("INSERT INTO meta(key, value) VALUES (?, ?)")
      Seq(
        "schema_version" -> SchemaVersion.toString,
        "kb_root" -> KbPath.render(kb.root),
        "built_at" -> Instant.now().toString
      ).foreach { (k, v) =>
        metaSt.setString(1, k); metaSt.setString(2, v); metaSt.addBatch()
      }
      metaSt.executeBatch(); metaSt.close()

      val bundleSt = conn.prepareStatement(
        "INSERT INTO bundle(id, label, name, grp, okf_version, title, description, root_path) VALUES (?,?,?,?,?,?,?,?)"
      )
      val docSt = conn.prepareStatement(
        """INSERT INTO doc(id, bundle_id, bundle_path, rel_path, file_path, kind, type, title, description,
                           status, stale_after, resource, has_frontmatter, frontmatter_error, body_lines, body_chars)
           VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"""
      )
      val ftsSt = conn.prepareStatement("INSERT INTO doc_fts(rowid, bundle_path, title, description, body) VALUES (?,?,?,?,?)")
      val tagSt = conn.prepareStatement("INSERT OR IGNORE INTO tag(doc_id, tag) VALUES (?,?)")
      val fmSt = conn.prepareStatement("INSERT OR IGNORE INTO frontmatter(doc_id, key, value) VALUES (?,?,?)")
      val srcSt = conn.prepareStatement(
        "INSERT INTO source(doc_id, source_id, resource, title, org, repo, commit_sha, src_path) VALUES (?,?,?,?,?,?,?,?)"
      )
      val headSt = conn.prepareStatement("INSERT INTO heading(doc_id, level, text, slug, line) VALUES (?,?,?,?,?)")
      val linkSt = conn.prepareStatement("INSERT INTO link(doc_id, dest, text, line, kind) VALUES (?,?,?,?,?)")

      var docId = 0
      var links = 0
      var heads = 0
      var sources = 0
      var tags = 0
      // (bundleId, bundlePath) -> docId, for resolving internal links in a second pass.
      val docIds = collection.mutable.Map.empty[(Int, String), Int]

      kb.bundles.zipWithIndex.foreach { (b, bIdx) =>
        val bundleId = bIdx + 1
        bundleSt.setInt(1, bundleId)
        bundleSt.setString(2, b.label)
        bundleSt.setString(3, b.name)
        setOpt(bundleSt, 4, b.group)
        setOpt(bundleSt, 5, b.okfVersion)
        setOpt(bundleSt, 6, b.index.fm.title)
        setOpt(bundleSt, 7, b.index.fm.description)
        bundleSt.setString(8, KbPath.render(b.root))
        bundleSt.addBatch()

        b.allDocs.foreach { d =>
          docId += 1
          val id = docId
          docIds((bundleId, d.bundlePath)) = id

          docSt.setInt(1, id)
          docSt.setInt(2, bundleId)
          docSt.setString(3, d.bundlePath)
          docSt.setString(4, d.rel.mkString("/"))
          docSt.setString(5, KbPath.render(d.file))
          docSt.setString(6, d.kind.toString)
          setOpt(docSt, 7, d.fm.`type`)
          setOpt(docSt, 8, d.fm.title.orElse(Some(d.displayTitle)))
          setOpt(docSt, 9, d.fm.description)
          setOpt(docSt, 10, d.fm.status)
          setOpt(docSt, 11, d.fm.staleAfter)
          setOpt(docSt, 12, d.fm.resource)
          docSt.setInt(13, if d.hasFrontmatterBlock then 1 else 0)
          setOpt(docSt, 14, d.frontmatterError)
          docSt.setInt(15, d.body.linesIterator.size)
          docSt.setInt(16, d.body.length)
          docSt.addBatch()

          ftsSt.setInt(1, id)
          ftsSt.setString(2, d.bundlePath)
          ftsSt.setString(3, d.fm.title.getOrElse(d.displayTitle))
          ftsSt.setString(4, d.fm.description.getOrElse(""))
          ftsSt.setString(5, d.body)
          ftsSt.addBatch()

          d.fm.tags.foreach { t =>
            tagSt.setInt(1, id); tagSt.setString(2, t); tagSt.addBatch(); tags += 1
          }

          d.fm.values.foreach { (k, v) =>
            flatten(v).foreach { text =>
              fmSt.setInt(1, id); fmSt.setString(2, k); fmSt.setString(3, text); fmSt.addBatch()
            }
          }

          d.fm.sources.foreach { s =>
            srcSt.setInt(1, id)
            setOpt(srcSt, 2, s.id)
            srcSt.setString(3, s.resource)
            setOpt(srcSt, 4, s.title)
            s.resource match
              case GitHubUrl(org, repo, sha, path) =>
                srcSt.setString(5, org); srcSt.setString(6, repo)
                srcSt.setString(7, sha); srcSt.setString(8, path)
              case _ =>
                (5 to 8).foreach(srcSt.setNull(_, java.sql.Types.VARCHAR))
            srcSt.addBatch(); sources += 1
          }

          headings(d.body).foreach { (level, text, line) =>
            headSt.setInt(1, id); headSt.setInt(2, level); headSt.setString(3, text)
            headSt.setString(4, slugify(text)); headSt.setInt(5, line)
            headSt.addBatch(); heads += 1
          }

          d.links.foreach { l =>
            linkSt.setInt(1, id); linkSt.setString(2, l.dest); linkSt.setString(3, l.text)
            linkSt.setInt(4, l.line); linkSt.setString(5, linkKind(l))
            linkSt.addBatch(); links += 1
          }
        }
      }

      Seq(bundleSt, docSt, ftsSt, tagSt, fmSt, srcSt, headSt, linkSt).foreach { s => s.executeBatch(); s.close() }

      // Second pass: resolve bundle-relative links to their target doc. Only same-bundle destinations resolve,
      // which is exactly what a bundle-relative path means.
      val resolve = conn.prepareStatement(
        """UPDATE link
             SET target_doc_id = (
               SELECT d.id FROM doc d
               WHERE d.bundle_id = (SELECT bundle_id FROM doc s WHERE s.id = link.doc_id)
                 AND d.bundle_path = CASE
                   WHEN instr(link.dest, '#') > 0 THEN substr(link.dest, 1, instr(link.dest, '#') - 1)
                   ELSE link.dest
                 END
             )
           WHERE kind = 'bundle'"""
      )
      resolve.executeUpdate(); resolve.close()

      conn.commit()
      conn.createStatement().execute("ANALYZE")
      IndexStats(kb.bundles.size, docId, kb.concepts.size, links, heads, sources, tags)
    finally conn.close()

  /** Renders a frontmatter value as text. Nested mappings are skipped — they have no single sensible scalar form,
    * and the structures that matter (`sources`) already have dedicated tables.
    */
  private def flatten(v: Any): Option[String] =
    import scala.jdk.CollectionConverters.*
    v match
      case null => None
      case _: java.util.Map[?, ?] => None
      case l: java.util.List[?] =>
        val parts = l.asScala.toSeq.collect { case s: String => s; case n: Number => n.toString }
        Option.when(parts.nonEmpty)(parts.mkString(", "))
      case other => Some(other.toString)

  private def setOpt(st: java.sql.PreparedStatement, i: Int, v: Option[String]): Unit =
    v match
      case Some(s) => st.setString(i, s)
      case None => st.setNull(i, java.sql.Types.VARCHAR)

  // ------------------------------------------------------------------ query

  case class Rows(columns: Seq[String], rows: Seq[Seq[Option[String]]])

  /** Runs a read-only query. Anything that is not a SELECT/WITH/PRAGMA/EXPLAIN is refused — this is a query surface,
    * not a way to mutate derived state behind the builder's back.
    */
  def query(db: Path, sql: String): Either[String, Rows] < (Sync & Abort[Throwable]) =
    val trimmed = sql.trim.stripSuffix(";").trim
    val head = trimmed.takeWhile(c => !c.isWhitespace).toLowerCase
    if !Set("select", "with", "pragma", "explain").contains(head) then
      (Left(s"refusing to run `$head`: kb query is read-only (SELECT, WITH, PRAGMA, EXPLAIN)"): Either[String, Rows] < (Sync & Abort[Throwable]))
    else
      db.exists.map {
        case false => Left(s"no index at ${KbPath.render(db)} — run `kb index` first")
        case true =>
          Sync.defer {
            val conn = connect(db)
            try
              val st = conn.createStatement()
              val rs = st.executeQuery(trimmed)
              Right(readAll(rs))
            catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString))
            finally conn.close()
          }
      }

  private def readAll(rs: ResultSet): Rows =
    val md = rs.getMetaData
    val cols = (1 to md.getColumnCount).map(md.getColumnLabel)
    val buf = collection.mutable.ArrayBuffer.empty[Seq[Option[String]]]
    while rs.next() do
      buf += (1 to md.getColumnCount).map { i =>
        Option(rs.getString(i))
      }
    Rows(cols, buf.toSeq)

  /** Full-text search over titles, descriptions and bodies, ranked by FTS5's bm25. */
  def search(db: Path, needle: String, limit: Int): Either[String, Rows] < (Sync & Abort[Throwable]) =
    val escaped = needle.replace("'", "''")
    query(
      db,
      s"""SELECT b.label AS bundle, d.bundle_path, d.type, d.status, d.title, d.description,
                 snippet(doc_fts, 3, '[', ']', '…', 12) AS snippet
          FROM doc_fts
          JOIN doc d ON d.id = doc_fts.rowid
          JOIN bundle b ON b.id = d.bundle_id
          WHERE doc_fts MATCH '$escaped'
          ORDER BY bm25(doc_fts, 1.0, 8.0, 4.0, 1.0)
          LIMIT $limit"""
    )

  /** When the index was built, and whether any markdown file has changed since. */
  def status(db: Path, kb: Kb): Either[String, (String, Int, Seq[String])] < (Sync & Abort[Throwable]) =
    db.exists.map {
      case false => Left(s"no index at ${KbPath.render(db)}")
      case true =>
        for
          built <- Sync.defer {
            val conn = connect(db)
            try
              val rs = conn.createStatement().executeQuery("SELECT value FROM meta WHERE key = 'built_at'")
              if rs.next() then Option(rs.getString(1)) else None
            catch case _: Exception => None
            finally conn.close()
          }
          indexed <- Sync.defer {
            val conn = connect(db)
            try
              val rs = conn.createStatement().executeQuery("SELECT count(*) FROM doc")
              if rs.next() then Some(rs.getInt(1)) else None
            catch case _: Exception => None
            finally conn.close()
          }
          stale <- staleFiles(kb, built.map(Instant.parse))
        yield
          val current = kb.bundles.map(_.allDocs.size).sum
          // Modification times only speak for files that still exist. A deleted document leaves no mtime to compare,
          // so without a count check the index would keep serving it as though it were still there.
          val counts = indexed.filterNot(_ == current).toSeq.map { n =>
            val verb = if n > current then s"${n - current} document(s) removed since" else s"${current - n} document(s) added since"
            s"($verb the build)"
          }
          built match
            case None => Left("index has no built_at — rebuild it")
            case Some(b) => Right((b, current, stale ++ counts))
    }

  private def staleFiles(kb: Kb, builtAt: Option[Instant]): Seq[String] < (Sync & Abort[Throwable]) =
    builtAt match
      case None => (Seq.empty[String]: Seq[String] < (Sync & Abort[Throwable]))
      case Some(at) =>
        Kyo.foreach(Chunk.from(kb.bundles.flatMap(_.allDocs))) { d =>
          d.file.stat.map(st => Option.when(Instant.ofEpochMilli(st.lastModifiedMs).isAfter(at))(kb.rel(d.file)))
        }.map(_.flatten.toSeq.sortBy(identity[String]))

  // -------------------------------------------------------------- rendering

  def renderRows(r: Rows, json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "columns" -> ujson.Arr.from(r.columns.map(ujson.Str(_))),
          "rowCount" -> r.rows.size,
          "rows" -> ujson.Arr.from(r.rows.map { row =>
            ujson.Obj.from(r.columns.zip(row).map((c, v) => c -> v.map(ujson.Str(_)).getOrElse(ujson.Null)))
          })
        ),
        indent = 2
      ) + "\n"
    else if r.rows.isEmpty then "no rows\n"
    else
      val widths = r.columns.indices.map { i =>
        (r.columns(i).length +: r.rows.map(_(i).getOrElse("").length)).max.min(70)
      }
      def fmt(cells: Seq[String]) =
        cells.zip(widths).map((c, w) => (if c.length > w then c.take(w - 1) + "…" else c).padTo(w, ' ')).mkString("  ").stripTrailing()
      val sb = StringBuilder()
      sb ++= fmt(r.columns) + "\n"
      sb ++= widths.map("-" * _).mkString("  ") + "\n"
      r.rows.foreach(row => sb ++= fmt(row.map(_.getOrElse(""))) + "\n")
      sb ++= s"\n${r.rows.size} row(s)\n"
      sb.toString

  def renderStats(s: IndexStats, db: Path, json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "db" -> KbPath.render(db),
          "schemaVersion" -> SchemaVersion,
          "bundles" -> s.bundles,
          "docs" -> s.docs,
          "concepts" -> s.concepts,
          "links" -> s.links,
          "headings" -> s.headings,
          "sources" -> s.sources,
          "tags" -> s.tags
        ),
        indent = 2
      ) + "\n"
    else
      s"""built ${KbPath.render(db)} (schema v$SchemaVersion)
         |  ${s.bundles} bundles, ${s.docs} docs (${s.concepts} concepts)
         |  ${s.links} links, ${s.headings} headings, ${s.sources} sources, ${s.tags} tags
         |""".stripMargin
