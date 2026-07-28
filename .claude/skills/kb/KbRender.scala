//| scalaVersion: 3.8.4
//| moduleDeps: [KbStore.scala, KbScaffold.scala]
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC5

/** Rendering for the `kb` commands — text for humans, JSON for machines.
  *
  * Takes primitives rather than the CLI option classes, so this stays independent of the caseapp layer.
  */

import kyo.*

object KbRender:

  private def pad(s: String, n: Int): String = if s.length >= n then s else s + " " * (n - s.length)

  // ------------------------------------------------------------------- list

  def listBundles(kb: Kb, json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "root" -> KbPath.render(kb.root),
          "bundles" -> ujson.Arr.from(kb.bundles.map { b =>
            ujson.Obj(
              "label" -> b.label,
              "name" -> b.name,
              "group" -> b.group.map(ujson.Str(_)).getOrElse(ujson.Null),
              "okfVersion" -> b.okfVersion.map(ujson.Str(_)).getOrElse(ujson.Null),
              "title" -> b.index.fm.title.map(ujson.Str(_)).getOrElse(ujson.Null),
              "description" -> b.index.fm.description.map(ujson.Str(_)).getOrElse(ujson.Null),
              "concepts" -> b.concepts.size,
              "subIndexes" -> b.subIndexes.size,
              "hasLog" -> b.log.isDefined
            )
          })
        ),
        indent = 2
      ) + "\n"
    else
      val sb = StringBuilder()
      val w = (kb.bundles.map(_.label.length) :+ 6).max
      sb ++= s"${pad("BUNDLE", w)}  OKF   CONCEPTS  TITLE\n"
      kb.bundles.foreach { b =>
        sb ++= s"${pad(b.label, w)}  ${pad(b.okfVersion.getOrElse("?"), 4)}  ${pad(b.concepts.size.toString, 8)}  ${b.index.fm.title.getOrElse("")}\n"
      }
      sb ++= s"\n${kb.bundles.size} bundle(s), ${kb.bundles.map(_.concepts.size).sum} concept(s)\n"
      if kb.strays.nonEmpty then sb ++= s"${kb.strays.size} stray markdown file(s) outside any bundle — run `kb check`\n"
      sb.toString

  def listConcepts(kb: Kb, b: Bundle, json: Boolean): String =
    if json then
      ujson.write(
        ujson.Obj(
          "bundle" -> b.label,
          "concepts" -> ujson.Arr.from(b.concepts.map(conceptJson))
        ),
        indent = 2
      ) + "\n"
    else
      val sb = StringBuilder()
      sb ++= s"${b.label} — ${b.index.fm.title.getOrElse("")}\n"
      b.index.fm.description.foreach(d => sb ++= s"$d\n")
      sb ++= "\n"
      val w = (b.concepts.map(_.bundlePath.length) :+ 4).max
      b.concepts.foreach { c =>
        val status = c.fm.status.map(s => s" [$s]").getOrElse("")
        sb ++= s"${pad(c.bundlePath, w)}  ${c.fm.`type`.getOrElse("?")}$status\n"
        sb ++= s"${" " * w}  ${c.fm.description.getOrElse("(no description)")}\n"
      }
      sb ++= s"\n${b.concepts.size} concept(s)"
      if b.subIndexes.nonEmpty then sb ++= s", ${b.subIndexes.size} sub-index(es)"
      sb ++= "\n"
      sb.toString

  private def conceptJson(c: Doc): ujson.Obj =
    ujson.Obj(
      "path" -> c.bundlePath,
      "file" -> KbPath.render(c.file),
      "type" -> c.fm.`type`.map(ujson.Str(_)).getOrElse(ujson.Null),
      "title" -> c.fm.title.map(ujson.Str(_)).getOrElse(ujson.Null),
      "description" -> c.fm.description.map(ujson.Str(_)).getOrElse(ujson.Null),
      "status" -> c.fm.status.map(ujson.Str(_)).getOrElse(ujson.Null),
      "tags" -> ujson.Arr.from(c.fm.tags.map(ujson.Str(_))),
      "sources" -> ujson.Arr.from(c.fm.sources.map(s => ujson.Str(s.resource)))
    )

  // ------------------------------------------------------------------- show

  def show(kb: Kb, path: String, bundleHint: Option[String], includeBody: Boolean, json: Boolean): String =
    findDoc(kb, path, bundleHint) match
      case None if json => ujson.write(ujson.Obj("found" -> false, "query" -> path), indent = 2) + "\n"
      case None => s"not found: $path\n"
      case Some((b, d)) if json =>
        val o = conceptJson(d)
        o("found") = true
        o("bundle") = b.label
        o("kind") = d.kind.toString
        o("links") = ujson.Arr.from(d.links.map(l => ujson.Obj("dest" -> l.dest, "line" -> l.line, "external" -> l.isExternal)))
        if includeBody then o("body") = d.body
        ujson.write(o, indent = 2) + "\n"
      case Some((b, d)) =>
        val sb = StringBuilder()
        sb ++= s"${b.label}${d.bundlePath}\n"
        sb ++= s"file:        ${KbPath.render(d.file)}\n"
        sb ++= s"kind:        ${d.kind}\n"
        d.fm.`type`.foreach(t => sb ++= s"type:        $t\n")
        d.fm.title.foreach(t => sb ++= s"title:       $t\n")
        d.fm.description.foreach(t => sb ++= s"description: $t\n")
        d.fm.status.foreach(t => sb ++= s"status:      $t\n")
        d.fm.staleAfter.foreach(t => sb ++= s"stale_after: $t\n")
        if d.fm.tags.nonEmpty then sb ++= s"tags:        ${d.fm.tags.mkString(", ")}\n"
        if d.fm.sources.nonEmpty then
          sb ++= "sources:\n"
          d.fm.sources.foreach(s => sb ++= s"  - ${s.id.map(i => s"$i: ").getOrElse("")}${s.resource}\n")
        val outbound = d.links.filter(l => !l.isExternal && !l.isAnchorOnly)
        if outbound.nonEmpty then
          sb ++= s"\noutbound links (${outbound.size}):\n"
          outbound.foreach(l => sb ++= s"  ${l.dest}\n")
        val headings = d.body.linesIterator.filter(_.startsWith("#")).toSeq
        if headings.nonEmpty then
          sb ++= s"\noutline:\n"
          headings.foreach(h => sb ++= s"  $h\n")
        if includeBody then sb ++= s"\n---\n${d.body}\n"
        sb.toString

  private def findDoc(kb: Kb, path: String, bundleHint: Option[String]): Option[(Bundle, Doc)] =
    val candidates = bundleHint.flatMap(kb.bundle).map(Seq(_)).getOrElse(kb.bundles)
    if path.startsWith("/") then
      candidates.flatMap(b => b.conceptAt(path).map(b -> _)).headOption
        .orElse(candidates.flatMap(b => b.allDocs.find(_.bundlePath == path).map(b -> _)).headOption)
    else
      val needle = path.stripSuffix("/")
      kb.bundles
        .flatMap(b => b.allDocs.map(b -> _))
        .find((_, d) => KbPath.render(d.file).endsWith(needle) || d.rel.mkString("/") == needle)

  // ----------------------------------------------------------------- search

  def search(
      kb: Kb,
      query: Option[String],
      searchBody: Boolean,
      typeFilter: Option[String],
      tagFilters: Seq[String],
      statusFilter: Option[String],
      bundleFilter: Option[String],
      json: Boolean
  ): String =
    val scope = bundleFilter.flatMap(kb.bundle).map(b => b.concepts.map(b -> _)).getOrElse(kb.concepts)
    val q = query.map(_.toLowerCase)

    def metaHit(d: Doc): Boolean = q.forall { needle =>
      Seq(d.fm.title, d.fm.description, d.fm.`type`).flatten.exists(_.toLowerCase.contains(needle)) ||
      d.fm.tags.exists(_.toLowerCase.contains(needle)) ||
      d.rel.toString.toLowerCase.contains(needle)
    }

    def bodyHits(d: Doc): Seq[(Int, String)] =
      q.filter(_ => searchBody).toSeq.flatMap { needle =>
        d.body.linesIterator.zipWithIndex.collect {
          case (l, i) if l.toLowerCase.contains(needle) => (i + 1, l.trim)
        }.toSeq
      }

    val matched = scope.filter { (_, d) =>
      typeFilter.forall(t => d.fm.`type`.exists(_.equalsIgnoreCase(t))) &&
      statusFilter.forall(s => d.fm.status.exists(_.equalsIgnoreCase(s))) &&
      tagFilters.forall(t => d.fm.tags.exists(_.equalsIgnoreCase(t))) &&
      (metaHit(d) || bodyHits(d).nonEmpty)
    }

    if json then
      ujson.write(
        ujson.Obj(
          "matches" -> matched.size,
          "results" -> ujson.Arr.from(matched.map { (b, d) =>
            val o = conceptJson(d)
            o("bundle") = b.label
            o("bodyHits") = ujson.Arr.from(bodyHits(d).map((n, l) => ujson.Obj("line" -> n, "text" -> l)))
            o
          })
        ),
        indent = 2
      ) + "\n"
    else
      val sb = StringBuilder()
      matched.foreach { (b, d) =>
        sb ++= s"${b.label}${d.bundlePath}  [${d.fm.`type`.getOrElse("?")}${d.fm.status.map(s => s", $s").getOrElse("")}]\n"
        sb ++= s"  ${d.fm.title.getOrElse(d.displayTitle)} — ${d.fm.description.getOrElse("")}\n"
        val hits = bodyHits(d)
        hits.take(3).foreach((n, l) => sb ++= s"  ${n}: ${l.take(140)}\n")
        if hits.size > 3 then sb ++= s"  … ${hits.size - 3} more line(s)\n"
      }
      if matched.isEmpty then sb ++= "no matches\n"
      else sb ++= s"\n${matched.size} match(es)\n"
      sb.toString

  // --------------------------------------------------------------- scaffold

  def scaffold(r: ScaffoldResult, json: Boolean): String =
    if json then
      return ujson.write(
        ujson.Obj(
          "created" -> ujson.Arr.from(r.created.map(p => ujson.Str(KbPath.render(p)))),
          "updated" -> ujson.Arr.from(r.updated.map(p => ujson.Str(KbPath.render(p)))),
          "notes" -> ujson.Arr.from(r.notes.map(ujson.Str(_)))
        ),
        indent = 2
      ) + "\n"
    val sb = StringBuilder()
    r.created.foreach(p => sb ++= s"created  ${KbPath.render(p)}\n")
    r.updated.foreach(p => sb ++= s"updated  ${KbPath.render(p)}\n")
    r.notes.foreach(n => sb ++= s"note     $n\n")
    sb ++= "\nnext: write the body, then run `kb check`\n"
    sb.toString
