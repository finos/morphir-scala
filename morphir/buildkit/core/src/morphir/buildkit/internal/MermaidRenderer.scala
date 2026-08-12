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
 *   - '''Stage.''' A [[SealedElem.StageNode]] declares as `id["label"]` — `id` its assigned [[morphir.buildkit.NodeId]]
 *     rendered, `label` its stage's own label or `<anonymous>` when unlabelled. A literal `"` inside a label is escaped
 *     to Mermaid's `#quot;` entity; that is the only escaping this renderer performs.
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
 *   - '''Determinism.''' Edges are emitted in definition order, after every node is declared; `\n`-joined, two-space
 *     indented, no trailing whitespace anywhere. Deterministic by construction — no sorting, no hashing, no iteration
 *     over an unordered collection.
 */
private[buildkit] object MermaidRenderer:

  /** Render `chain` as complete `flowchart TD` Mermaid source. See the layout rules on this object's own scaladoc. */
  def render(chain: SealedChain[?, ?, ?]): String =
    val elems = flatten(chain)
    val nodes = StringBuilder()
    declareChain(elems, "", nodes)
    val edges = StringBuilder()
    wireChain(elems, "", edges)
    val lines = (nodes.toString ++ edges.toString).linesIterator.filter(_.nonEmpty).toList
    ("flowchart TD" :: lines).mkString("\n")

  /** This chain's own elements, in definition order — `Append`'s snoc shape unrolled into a flat sequence. */
  private def flatten[I, O, S](chain: SealedChain[I, O, S]): Chunk[SealedElem[?, ?, ?]] =
    chain match
      case SealedChain.Single(elem)       => Chunk(elem)
      case SealedChain.Append(init, last) => flatten(init) :+ last

  /** The Mermaid id for a node assigned `id`, qualified by the enclosing fan-out's own id chain, if any. */
  private def mid(prefix: String, id: NodeId): String =
    if prefix.isEmpty then id.render else s"${prefix}_${id.render}"

  private def escape(label: String): String = label.replace("\"", "#quot;")

  private def labelOf(stage: Stage[?, ?, ?]): String =
    stage.label match
      case Present(l) => l
      case Absent     => "<anonymous>"

  private def isFanOut(e: SealedElem[?, ?, ?]): Boolean =
    e match
      case SealedElem.FanOutNode(_, _) => true
      case _                           => false

  private def declareChain(elems: Chunk[SealedElem[?, ?, ?]], prefix: String, sb: StringBuilder): Unit =
    elems.foreach(e => declareElem(e, prefix, sb))

  private def declareElem(e: SealedElem[?, ?, ?], prefix: String, sb: StringBuilder): Unit =
    e match
      case SealedElem.StageNode(id, stage) =>
        sb ++= s"  ${mid(prefix, id)}[\"${escape(labelOf(stage))}\"]\n"
      case SealedElem.ParNode(left, right, _) =>
        declareChain(flatten(left), prefix, sb)
        declareChain(flatten(right), prefix, sb)
      case SealedElem.FanOutNode(id, each) =>
        val ownId = mid(prefix, id)
        sb ++= s"  subgraph $ownId\n"
        declareChain(flatten(each), ownId, sb)
        sb ++= "  end\n"
      case SealedElem.BranchNode(id, _, ifTrue, ifFalse) =>
        sb ++= s"  ${mid(prefix, id)}{\"?\"}\n"
        declareChain(flatten(ifTrue), prefix, sb)
        declareChain(flatten(ifFalse), prefix, sb)

  /** The Mermaid ids a predecessor's edge should target when connecting into `e`. */
  private def entryOf(e: SealedElem[?, ?, ?], prefix: String): Chunk[String] =
    e match
      case SealedElem.StageNode(id, _)        => Chunk(mid(prefix, id))
      case SealedElem.ParNode(left, right, _) => entryOfChain(left, prefix) ++ entryOfChain(right, prefix)
      case SealedElem.FanOutNode(id, _)       => Chunk(mid(prefix, id))
      case SealedElem.BranchNode(id, _, _, _) => Chunk(mid(prefix, id))

  /** The Mermaid ids a successor's edge should originate from when connecting out of `e`. */
  private def exitOf(e: SealedElem[?, ?, ?], prefix: String): Chunk[String] =
    e match
      case SealedElem.StageNode(id, _)                  => Chunk(mid(prefix, id))
      case SealedElem.ParNode(left, right, _)           => exitOfChain(left, prefix) ++ exitOfChain(right, prefix)
      case SealedElem.FanOutNode(id, _)                 => Chunk(mid(prefix, id))
      case SealedElem.BranchNode(_, _, ifTrue, ifFalse) => exitOfChain(ifTrue, prefix) ++ exitOfChain(ifFalse, prefix)

  private def firstElem[I, O, S](chain: SealedChain[I, O, S]): SealedElem[?, ?, ?] =
    chain match
      case SealedChain.Single(elem)    => elem
      case SealedChain.Append(init, _) => firstElem(init)

  private def lastElem[I, O, S](chain: SealedChain[I, O, S]): SealedElem[?, ?, ?] =
    chain match
      case SealedChain.Single(elem)    => elem
      case SealedChain.Append(_, last) => last

  private def entryOfChain[I, O, S](chain: SealedChain[I, O, S], prefix: String): Chunk[String] =
    entryOf(firstElem(chain), prefix)

  private def exitOfChain[I, O, S](chain: SealedChain[I, O, S], prefix: String): Chunk[String] =
    exitOf(lastElem(chain), prefix)

  private def edgeLine(from: String, to: String, label: Maybe[String]): String =
    label match
      case Present(l) => s"  $from -->|$l| $to\n"
      case Absent     => s"  $from --> $to\n"

  private def wireChain(elems: Chunk[SealedElem[?, ?, ?]], prefix: String, sb: StringBuilder): Unit =
    elems.zipWithIndex.foreach { case (e, i) =>
      if i > 0 then
        val label = if isFanOut(e) then Present("per element") else Absent
        exitOf(elems(i - 1), prefix).foreach(from => entryOf(e, prefix).foreach(to => sb ++= edgeLine(from, to, label)))
      wireInternal(e, prefix, sb)
    }

  /** Edges internal to a composite element: a branch's own diamond-to-arm edges, and both kinds' nested chains. */
  private def wireInternal(e: SealedElem[?, ?, ?], prefix: String, sb: StringBuilder): Unit =
    e match
      case SealedElem.StageNode(_, _)         => ()
      case SealedElem.ParNode(left, right, _) =>
        wireChain(flatten(left), prefix, sb)
        wireChain(flatten(right), prefix, sb)
      case SealedElem.FanOutNode(id, each) =>
        wireChain(flatten(each), mid(prefix, id), sb)
      case SealedElem.BranchNode(id, _, ifTrue, ifFalse) =>
        val ownId = mid(prefix, id)
        entryOfChain(ifTrue, prefix).foreach(t => sb ++= s"  $ownId -->|true| $t\n")
        entryOfChain(ifFalse, prefix).foreach(t => sb ++= s"  $ownId -->|false| $t\n")
        wireChain(flatten(ifTrue), prefix, sb)
        wireChain(flatten(ifFalse), prefix, sb)
end MermaidRenderer
