package morphir.buildkit.internal

import kyo.*
import morphir.buildkit.*

/**
 * Renders a [[SealedChain]] as Mermaid `flowchart TD` source: a deterministic, structural view of the same graph
 * [[morphir.buildkit.SealedPipeline#execute]] walks. This is the first real consumer of the sealed shape besides
 * execution itself — [[morphir.buildkit.SealedPipeline#describe]] stays the compact one-line summary; `toMermaid` is
 * the diagram.
 *
 * '''Layout rules''' — fixed by this renderer, not negotiable per call:
 *
 *   - '''Two passes.''' Every node in the whole chain is declared before any edge is emitted, so the output is always
 *     structurally valid Mermaid regardless of forward references inside composite shapes.
 *   - '''Stage.''' A [[SealedElem.StageNode]] declares as `id["label"]` — `id` its assigned
 *     [[morphir.buildkit.NodeId]], sanitized (see below), `label` its stage's own label or `<anonymous>` when
 *     unlabelled, with a literal `"` escaped to Mermaid's `#quot;` entity.
 *   - '''Branch.''' A [[SealedElem.BranchNode]] declares as a decision diamond, `id{"?"}` — it carries no stage label
 *     of its own, only a predicate. Both arms' nodes declare inline (no id prefix: they already share the branch's own
 *     flat id namespace, guaranteed unique by sealing). Edges: the predecessor's own edge lands on the diamond like any
 *     ordinary node; the diamond then points at each arm's own first node, labelled `|true|` / `|false|`; each arm's
 *     own last node points at the branch's successor — both arms "reconverge" directly on whatever node follows the
 *     branch in the enclosing chain (nothing is drawn if the branch is the chain's last element).
 *   - '''Fan-out.''' A [[SealedElem.FanOutNode]] declares its child chain inside `subgraph id ... end`, with every
 *     child node id prefixed `id_` — a fresh, independent id namespace one level deeper, mirroring the executor's own
 *     per-element event-id qualification. The subgraph's own id doubles as its entry/exit anchor: Mermaid allows an
 *     edge to target a subgraph id directly, drawing to the cluster boundary. The edge in is labelled `|per element|`,
 *     since the child chain runs once per element of the incoming chunk; the edge out (if the fan-out has a successor)
 *     is a plain, unlabelled edge like any other.
 *   - '''Par.''' A [[SealedElem.ParNode]] carries no id of its own and declares nothing itself — only its
 *     `left`/`right` children's own nodes. Its entry is both sides' own entries and its exit is both sides' own exits,
 *     so the predecessor's edge fans out to both sides' entries (diverge) and both sides' exits fan into the
 *     successor's entry (reconverge), with no synthetic fork/join node in between.
 *   - '''Mermaid-id sanitization.''' [[morphir.buildkit.NodeId#segment]] permits characters Mermaid's own id syntax
 *     does not — spaces, brackets, quotes and more; only `/`, `\`, `.`/`..`, blank and control characters are rejected
 *     at seal time. Every id token this renderer emits (node ids, diamond ids, subgraph headers, edge endpoints)
 *     therefore goes through a two-step conversion, not just the label's own quote-escaping above: first every
 *     character outside `[A-Za-z0-9_-]` becomes `_`, and a result that starts with a digit is prefixed `n_` (Mermaid
 *     ids are unquoted bareword tokens; an unescaped `[`, `"` or leading digit inside one is not just ugly, it can fail
 *     to parse; `-` is kept as-is since slugified labels and the `node-N` position fallback already use it). Because
 *     two distinct ids can sanitize to the same string (`"a[1]"` and `"a 1 "` both become `a_1_`), collisions are
 *     resolved deterministically by first walking the whole chain — the same order node declaration itself walks in —
 *     and appending `_2`, `_3`, ... to whichever sanitized id was seen later, so the mapping from raw id to Mermaid id
 *     is a bijection and doesn't depend on which sibling happened to render first. Two distinct nodes always render two
 *     distinct labelled boxes; a label may still visually repeat (labels are display text, not identity), but the
 *     underlying `id[...]`/`id{...}` token behind each one never collides. Auto-derived ids — a slugified label, or the
 *     `node-N` position fallback — are already restricted to `[a-z0-9-]` and never trigger the fallback path in
 *     practice, but go through the same conversion unconditionally rather than being special-cased, so there is exactly
 *     one code path to reason about.
 *   - '''Determinism.''' Edges are emitted in definition order, after every node is declared; `\n`-joined, two-space
 *     indented, no trailing whitespace anywhere. Deterministic by construction — no sorting, no hashing, no iteration
 *     over an unordered collection; the id-sanitization map above is itself built by one deterministic left-to-right
 *     walk, not a hash-based collection.
 */
private[buildkit] object MermaidRenderer:

  /** Render `chain` as complete `flowchart TD` Mermaid source. See the layout rules on this object's own scaladoc. */
  def render(chain: SealedChain[?, ?, ?]): String =
    val elems = flatten(chain)
    val ids   = buildIdMap(collectRawIds(elems, ""))
    val nodes = StringBuilder()
    declareChain(elems, "", ids, nodes)
    val edges = StringBuilder()
    wireChain(elems, "", ids, edges)
    val lines = (nodes.toString ++ edges.toString).linesIterator.filter(_.nonEmpty).toList
    ("flowchart TD" :: lines).mkString("\n")

  /** This chain's own elements, in definition order — `Append`'s snoc shape unrolled into a flat sequence. */
  private def flatten[I, O, S](chain: SealedChain[I, O, S]): Chunk[SealedElem[?, ?, ?]] =
    chain match
      case SealedChain.Single(elem)       => Chunk(elem)
      case SealedChain.Append(init, last) => flatten(init) :+ last

  /**
   * The raw (pre-sanitization) id for a node assigned `id`, qualified by the enclosing fan-out's own raw id chain, if
   * any. Raw ids are already globally unique — distinct `NodeId`s are guaranteed by sealing, and fan-out nesting
   * qualifies each level with its own unique id — so this alone is a safe recursion prefix and a safe map key; only the
   * *rendered* form (see `buildIdMap`) needs deduplication, after characters unsafe for a Mermaid id are folded
   * together.
   */
  private def rawId(prefix: String, id: NodeId): String =
    if prefix.isEmpty then id.render else s"${prefix}_${id.render}"

  /**
   * Fold `raw` into a Mermaid-safe bareword: non-`[A-Za-z0-9_-]` characters become `_`, a leading digit gets `n_`. `-`
   * is kept as safe (not folded) because it is already load-bearing in this renderer's own auto-derived ids —
   * [[morphir.buildkit.internal.Sealing#slugify]]'s labels and the `node-N` position fallback both use it, and a fold
   * that touched those would change every id this renderer has always produced for the common, unproblematic case, not
   * just the pathological one this sanitization step exists for.
   */
  private def sanitize(raw: String): String =
    val cleaned = raw.map(c => if c.isLetterOrDigit || c == '_' || c == '-' then c else '_')
    if cleaned.headOption.exists(_.isDigit) then s"n_$cleaned" else cleaned

  /** The first of `base`, `base_2`, `base_3`, ... not already in `used` — deterministic, order-dependent on `used`. */
  private def firstFree(base: String, used: Set[String], suffix: Int = 1): String =
    val candidate = if suffix == 1 then base else s"${base}_$suffix"
    if used.contains(candidate) then firstFree(base, used, suffix + 1) else candidate

  /**
   * A bijection from every raw id in `rawIds` (definition order) to a unique, Mermaid-safe id: each raw id's own
   * `sanitize`d form, disambiguated with `firstFree` against every Mermaid id already assigned to an *earlier* raw id
   * in the same walk — so which of two colliding raw ids keeps the bare sanitized form depends only on declaration
   * order, never on iteration order over an unordered collection.
   */
  private def buildIdMap(rawIds: Chunk[String]): Map[String, String] =
    val (_, mapping) = rawIds.foldLeft((Set.empty[String], Map.empty[String, String])) { case ((used, acc), raw) =>
      val assigned = firstFree(sanitize(raw), used)
      (used + assigned, acc + (raw -> assigned))
    }
    mapping

  /** Every element's own raw id, walked in exactly the order [[declareChain]] declares nodes in. */
  private def collectRawIds(elems: Chunk[SealedElem[?, ?, ?]], prefix: String): Chunk[String] =
    elems.flatMap(e => collectRawIdsElem(e, prefix))

  private def collectRawIdsElem(e: SealedElem[?, ?, ?], prefix: String): Chunk[String] =
    e match
      case SealedElem.StageNode(id, _)        => Chunk(rawId(prefix, id))
      case SealedElem.ParNode(left, right, _) =>
        collectRawIds(flatten(left), prefix) ++ collectRawIds(flatten(right), prefix)
      case SealedElem.FanOutNode(id, each) =>
        val ownId = rawId(prefix, id)
        Chunk(ownId) ++ collectRawIds(flatten(each), ownId)
      case SealedElem.BranchNode(id, _, ifTrue, ifFalse) =>
        Chunk(rawId(prefix, id)) ++ collectRawIds(flatten(ifTrue), prefix) ++ collectRawIds(flatten(ifFalse), prefix)

  /** The Mermaid id actually rendered for a node assigned `id`, resolved through the id map built up-front. */
  private def mid(prefix: String, id: NodeId, ids: Map[String, String]): String = ids(rawId(prefix, id))

  private def escape(label: String): String = label.replace("\"", "#quot;")

  private def labelOf(stage: Stage[?, ?, ?]): String =
    stage.label match
      case Present(l) => l
      case Absent     => "<anonymous>"

  private def isFanOut(e: SealedElem[?, ?, ?]): Boolean =
    e match
      case SealedElem.FanOutNode(_, _) => true
      case _                           => false

  private def declareChain(
      elems: Chunk[SealedElem[?, ?, ?]],
      prefix: String,
      ids: Map[String, String],
      sb: StringBuilder
  ): Unit =
    elems.foreach(e => declareElem(e, prefix, ids, sb))

  private def declareElem(e: SealedElem[?, ?, ?], prefix: String, ids: Map[String, String], sb: StringBuilder): Unit =
    e match
      case SealedElem.StageNode(id, stage) =>
        sb ++= s"  ${mid(prefix, id, ids)}[\"${escape(labelOf(stage))}\"]\n"
      case SealedElem.ParNode(left, right, _) =>
        declareChain(flatten(left), prefix, ids, sb)
        declareChain(flatten(right), prefix, ids, sb)
      case SealedElem.FanOutNode(id, each) =>
        val ownId    = mid(prefix, id, ids)
        val ownRawId = rawId(prefix, id)
        sb ++= s"  subgraph $ownId\n"
        declareChain(flatten(each), ownRawId, ids, sb)
        sb ++= "  end\n"
      case SealedElem.BranchNode(id, _, ifTrue, ifFalse) =>
        sb ++= s"  ${mid(prefix, id, ids)}{\"?\"}\n"
        declareChain(flatten(ifTrue), prefix, ids, sb)
        declareChain(flatten(ifFalse), prefix, ids, sb)

  /** The Mermaid ids a predecessor's edge should target when connecting into `e`. */
  private def entryOf(e: SealedElem[?, ?, ?], prefix: String, ids: Map[String, String]): Chunk[String] =
    e match
      case SealedElem.StageNode(id, _)        => Chunk(mid(prefix, id, ids))
      case SealedElem.ParNode(left, right, _) => entryOfChain(left, prefix, ids) ++ entryOfChain(right, prefix, ids)
      case SealedElem.FanOutNode(id, _)       => Chunk(mid(prefix, id, ids))
      case SealedElem.BranchNode(id, _, _, _) => Chunk(mid(prefix, id, ids))

  /** The Mermaid ids a successor's edge should originate from when connecting out of `e`. */
  private def exitOf(e: SealedElem[?, ?, ?], prefix: String, ids: Map[String, String]): Chunk[String] =
    e match
      case SealedElem.StageNode(id, _)        => Chunk(mid(prefix, id, ids))
      case SealedElem.ParNode(left, right, _) => exitOfChain(left, prefix, ids) ++ exitOfChain(right, prefix, ids)
      case SealedElem.FanOutNode(id, _)       => Chunk(mid(prefix, id, ids))
      case SealedElem.BranchNode(_, _, ifTrue, ifFalse) =>
        exitOfChain(ifTrue, prefix, ids) ++ exitOfChain(ifFalse, prefix, ids)

  private def firstElem[I, O, S](chain: SealedChain[I, O, S]): SealedElem[?, ?, ?] =
    chain match
      case SealedChain.Single(elem)    => elem
      case SealedChain.Append(init, _) => firstElem(init)

  private def lastElem[I, O, S](chain: SealedChain[I, O, S]): SealedElem[?, ?, ?] =
    chain match
      case SealedChain.Single(elem)    => elem
      case SealedChain.Append(_, last) => last

  private def entryOfChain[I, O, S](
      chain: SealedChain[I, O, S],
      prefix: String,
      ids: Map[String, String]
  ): Chunk[String] =
    entryOf(firstElem(chain), prefix, ids)

  private def exitOfChain[I, O, S](
      chain: SealedChain[I, O, S],
      prefix: String,
      ids: Map[String, String]
  ): Chunk[String] =
    exitOf(lastElem(chain), prefix, ids)

  private def edgeLine(from: String, to: String, label: Maybe[String]): String =
    label match
      case Present(l) => s"  $from -->|$l| $to\n"
      case Absent     => s"  $from --> $to\n"

  private def wireChain(
      elems: Chunk[SealedElem[?, ?, ?]],
      prefix: String,
      ids: Map[String, String],
      sb: StringBuilder
  ): Unit =
    elems.zipWithIndex.foreach { case (e, i) =>
      if i > 0 then
        val label = if isFanOut(e) then Present("per element") else Absent
        exitOf(elems(i - 1), prefix, ids).foreach(from =>
          entryOf(e, prefix, ids).foreach(to => sb ++= edgeLine(from, to, label))
        )
      wireInternal(e, prefix, ids, sb)
    }

  /** Edges internal to a composite element: a branch's own diamond-to-arm edges, and both kinds' nested chains. */
  private def wireInternal(e: SealedElem[?, ?, ?], prefix: String, ids: Map[String, String], sb: StringBuilder): Unit =
    e match
      case SealedElem.StageNode(_, _)         => ()
      case SealedElem.ParNode(left, right, _) =>
        wireChain(flatten(left), prefix, ids, sb)
        wireChain(flatten(right), prefix, ids, sb)
      case SealedElem.FanOutNode(id, each) =>
        wireChain(flatten(each), rawId(prefix, id), ids, sb)
      case SealedElem.BranchNode(id, _, ifTrue, ifFalse) =>
        val ownId = mid(prefix, id, ids)
        entryOfChain(ifTrue, prefix, ids).foreach(t => sb ++= s"  $ownId -->|true| $t\n")
        entryOfChain(ifFalse, prefix, ids).foreach(t => sb ++= s"  $ownId -->|false| $t\n")
        wireChain(flatten(ifTrue), prefix, ids, sb)
        wireChain(flatten(ifFalse), prefix, ids, sb)
end MermaidRenderer
