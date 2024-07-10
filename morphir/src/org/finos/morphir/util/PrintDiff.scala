package org.finos.morphir.util

object PrintDiff {
  def apply(value: Compare.Diff) = new PrintDiff().apply(value)
}

private class PrintDiff(val defaultWidth: Int = 150) extends pprint.Walker {
  import fansi.Str
  import pprint.Tree.{Apply, Infix, KeyValue, Lazy}
  import pprint.{Renderer, Tree, Truncated}

  import java.util.function.UnaryOperator

  // Show field names e.g.
  // Property(ir = Ident(name = "inst"), name = "currentDate") vs
  // Property(Ident("inst"), "currentDate")
  val showFieldNames                = false
  val escapeUnicode                 = false
  val defaultHeight: Int            = Integer.MAX_VALUE
  val defaultIndent: Int            = 2
  val colorLiteral: fansi.Attrs     = fansi.Color.Green
  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow
  var verbose: Boolean              = false

  override def additionalHandlers: PartialFunction[Any, Tree] = PartialFunction.empty

  def apply(x: Any, verbose: Boolean = false): fansi.Str = {
    this.verbose = verbose
    val tokenized = this.tokenize(x).toSeq
    fansi.Str.join(tokenized)
  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree      = this.treeify(x)
    val renderer  = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered  = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }

  def treeify(x: Any): Tree = this.treeify(x, escapeUnicode, showFieldNames)

  /**
   * In the situation where one tree is subtantially larger than another (e.g. 1-line vs 50-lines) it typically does not
   * make sense to show the larger tree in its entirely because the difference is at the 'tips' of the trees. In the
   * case of MorphirJsonDecoding specs showing the larger tree in its entirety would introduce such a large amount of
   * noise that the logs became impractical to use.
   */
  def truncateSmallerTree(aRaw: Tree, bRaw: Tree) = {
    // Need to make duplicates to do countMaxDepth because it is destructive because the Tree has mutable iterators
    val (a, aDup) = duplicateTree(aRaw)
    val (b, bDup) = duplicateTree(bRaw)

    val (smallLabel, smallTree, smallDepth) :: (largeLabel, largeTree, largeDepth) :: Nil =
      (List(("a", a, countMaxDepth(aDup)), ("b", b, countMaxDepth(bDup))).sortBy(_._3)): @unchecked

    // Activate this logic if one tree is 5x as large as the other or bigger
    val largeTreeTruncated =
      if (largeDepth >= smallDepth * 5) {
        truncateDepth(largeTree, smallDepth * 5)
      } else {
        largeTree
      }

    // put them back into a/b order by sort by the labels which will be either "a", or "b"
    val (_, aOut) :: (_, bOut) :: Nil =
      // NOTE: Please verify this is correct had to be marked with @unchecked to eliminate a warning
      (List((smallLabel, smallTree), (largeLabel, largeTreeTruncated)).sortBy(_._1)): @unchecked

    (aOut, bOut)
  }

  def truncateDepth(tree: Tree, truncateDepth: Int) = {
    def truncateDepthRec(tree: Tree, currDepth: Int): Tree =
      if (currDepth >= truncateDepth)
        Tree.Literal("...")
      else
        tree match {
          case Apply(prefix, body) => Apply(prefix, body.toList.map(truncateDepthRec(_, currDepth + 1)).iterator)
          case l @ Lazy(_)         => l // don't do anything ro lazy values
          case Infix(lhs, op, rhs) =>
            Infix(truncateDepthRec(lhs, currDepth + 1), op, truncateDepthRec(rhs, currDepth + 1))
          case l @ Tree.Literal(_)  => l
          case KeyValue(key, value) => KeyValue(key, truncateDepthRec(value, currDepth + 1))
        }

    truncateDepthRec(tree, 0)
  }

  def duplicateTree(tree: Tree): (Tree, Tree) =
    tree match {
      case Apply(prefix, body) =>
        val (aBody, bBody) = body.toList.map(duplicateTree(_)).unzip
        (Apply(prefix, aBody.iterator), Apply(prefix, bBody.iterator))

      case KeyValue(key, value) =>
        val (value1, value2) = duplicateTree(value)
        (KeyValue(key, value1), KeyValue(key, value2))

      case l @ Lazy(_) => (l, l)

      case Infix(lhs, op, rhs) =>
        val (lhs1, lhs2) = duplicateTree(lhs)
        val (rhs1, rhs2) = duplicateTree(rhs)
        (Infix(lhs1, op, rhs1), Infix(lhs2, op, rhs2))

      case l @ Tree.Literal(_) => (l, l)
    }

  // IMPORTANT - This function is actually destructive since it will make the Tree.Apply.body's iterator go to the end,
  // make sure to copy the tree before doing it
  def countMaxDepth(tree: Tree, currDepth: Int = 0): Int =
    tree match {
      case Apply(_, body)     => body.toList.map(countMaxDepth(_, currDepth + 1)).maxOption.getOrElse(0)
      case KeyValue(_, value) => countMaxDepth(value, currDepth + 1)
      case Lazy(_)            => currDepth // don't know what to do about lazy trees for now
      case Infix(lhs, _, rhs) => Math.max(countMaxDepth(lhs, currDepth + 1), countMaxDepth(rhs, currDepth + 1))
      case Tree.Literal(_)    => currDepth
    }

  override def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree = x match {
    case l: Compare.Diff.Leaf =>
      val leftRaw  = treeify(l.a, escapeUnicode, showFieldNames)
      val rightRaw = treeify(l.b, escapeUnicode, showFieldNames)
      // after invoking the below function cannot use leftRaw, rightRaw anymore because Tree.Apply uses iterators
      val (a, b) = truncateSmallerTree(leftRaw, rightRaw)
      Tree.Apply(
        "Diff",
        List(
          Tree.KeyValue("left", a),
          Tree.KeyValue("right", b)
        ).iterator
      )

    case l: Compare.Diff.Leaf2 =>
      val leftRaw  = treeify(l.a, escapeUnicode, showFieldNames)
      val rightRaw = treeify(l.b, escapeUnicode, showFieldNames)
      // after invoking the below function cannot use leftRaw, rightRaw anymore because Tree.Apply uses iterators
      val (a, b) = truncateSmallerTree(leftRaw, rightRaw)
      Tree.Apply(
        "Diff",
        List(
          Tree.KeyValue("left", a),
          Tree.KeyValue("right", b)
        ).iterator
      )

    case s: Compare.Diff.Set =>
      Tree.Apply(
        s.typename,
        List(
          Tree.Apply("onlyLeft", s.onlyLeft.toList.map(treeify(_, escapeUnicode, showFieldNames)).iterator),
          Tree.Apply("onlyRight", s.onlyRight.toList.map(treeify(_, escapeUnicode, showFieldNames)).iterator)
        ).iterator
      )

    case m: Compare.Diff.MissingLeft =>
      Tree.Apply(
        "Diff",
        List(
          Tree.KeyValue("left", Tree.Literal("Missing")),
          Tree.KeyValue("right", treeify(m.rightValue, escapeUnicode, showFieldNames))
        ).iterator
      )

    case m: Compare.Diff.MissingRight =>
      Tree.Apply(
        "Diff",
        List(
          Tree.KeyValue("left", treeify(m.leftValue, escapeUnicode, showFieldNames)),
          Tree.KeyValue("right", Tree.Literal("Missing"))
        ).iterator
      )

    case s: Compare.Diff.Sequence =>
      Tree.Apply(
        s.typename,
        s.fields.toList
          .sortBy(_._1)
          .map { case (key, diff) =>
            Tree.KeyValue(key, treeify(diff, escapeUnicode, showFieldNames))
          }
          .iterator
      )

    case o: Compare.Diff.Object =>
      Tree.Apply(
        o.typename,
        o.fields.map { case (name, value) =>
          Tree.KeyValue(name, treeify(value, escapeUnicode, showFieldNames))
        }.iterator
      )

    case other => super.treeify(other, escapeUnicode, showFieldNames)
  }
}
