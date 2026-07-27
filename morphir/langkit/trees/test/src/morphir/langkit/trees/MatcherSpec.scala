package morphir.langkit.trees

import kyo.test.*

import morphir.langkit.trees.ToyTree.*
import morphir.langkit.trees.query.*

class MatcherSpec extends Test[Any]:

  private def q(src: String): Query = QueryParser.parse(src) match
    case parsley.Success(x) => x
    case parsley.Failure(m) => throw new AssertionError(s"bad fixture: $m")

  private val xCap: CaptureName = CaptureName.make("x").toOption.get
  private val nCap: CaptureName = CaptureName.make("n").toOption.get
  private val bCap: CaptureName = CaptureName.make("b").toOption.get
  private val lCap: CaptureName = CaptureName.make("l").toOption.get

  // A small forest used across tests.
  private val leafHi: ToyTree  = Leaf("hi")
  private val leafBye: ToyTree = Leaf("bye")
  private val leafYo: ToyTree  = Leaf("yo")
  private val named: ToyTree   = Named(name = leafHi, body = leafBye)
  private val branch: ToyTree  = Branch(Seq(leafHi, named, leafYo))

  "Matcher" - {
    "structural matching" - {
      "a bare node pattern matches every node of that type" in {
        val ms = Matcher.matches(q("(Leaf)"), branch).toList
        assert(ms.size == 4) // leafHi, leafHi inside named, leafBye inside named, leafYo
      }
      "a wildcard matches every node" in {
        val ms = Matcher.matches(q("_"), named).toList
        assert(ms.size == 3) // named, leafHi, leafBye
      }
      "a wildcard can be captured" in {
        val ms = Matcher.matches(q("_ @x"), leafHi).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(xCap).contains(leafHi))
      }
      "no match yields an empty lazy list" in {
        val ms = Matcher.matches(q("(Branch)"), leafHi)
        assert(ms.isEmpty)
      }
      "multiple top-level patterns return combined matches" in {
        val ms = Matcher.matches(q("(Named) (Leaf)"), branch).toList
        assert(ms.size == 5)
      }
      "multiple top-level patterns preserve pattern-order grouping" in {
        val ms           = Matcher.matches(q("(Named) @n (Leaf) @l"), branch).toList
        val firstIsNamed = ms.head.captures.contains(nCap)
        val namedCount   = ms.count(_.captures.contains(nCap))
        val leafCount    = ms.count(_.captures.contains(lCap))
        val splitAt      = namedCount
        val grouped      = ms.take(splitAt).forall(_.captures.contains(nCap)) &&
          ms.drop(splitAt).forall(_.captures.contains(lCap))
        assert(firstIsNamed)
        assert(namedCount == 1)
        assert(leafCount == 4)
        assert(grouped)
      }
      "alternation matches either branch deterministically" in {
        val ms         = Matcher.matches(q("[(Named) @n (Leaf) @l]"), branch).toList
        val namedCount = ms.count(_.captures.contains(nCap))
        val leafCount  = ms.count(_.captures.contains(lCap))
        assert(ms.size == 5)
        assert(namedCount == 1)
        assert(leafCount == 4)
      }
      "alternation favors first matching branch for same node" in {
        val ms = Matcher.matches(q("[_ @n (Leaf) @l]"), leafHi).toList
        assert(ms.size == 1)
        assert(ms.head.captures.contains(nCap))
        assert(!ms.head.captures.contains(lCap))
      }
    }
    "fields" - {
      "field pattern constrains a named child" in {
        val ms = Matcher.matches(q("(Named name: (Leaf) @n)"), branch).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(nCap).contains(leafHi))
      }
      "negated field constraint matches when field is absent" in {
        val ms = Matcher.matches(q("(Leaf !name)"), branch).toList
        assert(ms.size == 4)
      }
      "field pattern with multiple fields binds each capture" in {
        val ms = Matcher.matches(q("(Named name: (Leaf) @n body: (Leaf) @b)"), branch).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(nCap).contains(leafHi))
        assert(ms.head.captures.get(bCap).contains(leafBye))
      }
      "negated field constraint fails when field is present" in {
        val ms = Matcher.matches(q("(Named !name)"), branch).toList
        assert(ms.isEmpty)
      }
      "field pattern fails when sub-pattern does not match" in {
        val ms = Matcher.matches(q("(Named name: (Branch))"), named).toList
        assert(ms.isEmpty)
      }
    }
    "ordered child matching" - {
      "unfielded child patterns match children in order" in {
        val ms = Matcher.matches(q("(Branch (Leaf) @n (Named) @b)"), branch).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(nCap).contains(leafHi))
        assert(ms.head.captures.get(bCap).contains(named))
      }
      "unfielded child patterns fail when ordered sequence cannot be found" in {
        val ms = Matcher.matches(q("(Branch (Named) @b (Named) @n)"), branch).toList
        assert(ms.isEmpty)
      }
      "field and unfielded child patterns can be mixed" in {
        val ms = Matcher.matches(q("(Named name: (Leaf) @n (Leaf) @b)"), named).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(nCap).contains(leafHi))
        assert(ms.head.captures.get(bCap).contains(leafBye))
      }
      "ordered child matching is deterministic when multiple alignments are possible" in {
        val root: ToyTree = Branch(Seq(Leaf("a"), Leaf("b"), Leaf("c")))
        val ms            = Matcher.matches(q("(Branch (Leaf) @n (Leaf) @b)"), root).toList
        val capN          = ms.head.captures(nCap)
        val capB          = ms.head.captures(bCap)
        assert(ms.size == 1)
        assert(capN == Leaf("a"))
        assert(capB == Leaf("b"))
      }
      "anchored child patterns require immediate sibling adjacency" in {
        val ms = Matcher.matches(q("(Branch (Leaf) @n . (Named) @b)"), branch).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(nCap).contains(leafHi))
        assert(ms.head.captures.get(bCap).contains(named))
      }
      "anchored child patterns fail when only non-adjacent match exists" in {
        val ms = Matcher.matches(q("(Branch (Leaf) @n . (Leaf) @b)"), branch).toList
        assert(ms.isEmpty)
      }
      "optional child quantifier allows missing child" in {
        val ms = Matcher.matches(q("(Named (Leaf) @n?)"), named).toList
        assert(ms.size == 1)
        assert(ms.head.captures.get(nCap).contains(leafHi))
      }
      "one-or-more child quantifier requires at least one match" in {
        val ms = Matcher.matches(q("(Named (Branch)+)"), named).toList
        assert(ms.isEmpty)
      }
      "zero-or-more child quantifier allows zero matches" in {
        val ms = Matcher.matches(q("(Named (Branch)*)"), named).toList
        assert(ms.size == 1)
      }
    }
    "predicates" - {
      "#eq? on text succeeds when both captures have identical text" in {
        val root: ToyTree = Branch(Seq(Leaf("same"), Leaf("same")))
        val pairQuery     = q("(Leaf) @l (#eq? @l \"same\")")
        val pairMs        = Matcher.matches(pairQuery, root).toList
        assert(pairMs.size == 2)
      }
      "#eq? fails when the capture text does not match the literal" in {
        val ms = Matcher.matches(q("(Leaf) @l (#eq? @l \"other\")"), leafHi).toList
        assert(ms.isEmpty)
      }
      "#match? passes captures whose text matches the regex" in {
        val ms = Matcher.matches(q("(Leaf) @l (#match? @l \"^h\")"), branch).toList
        // Only leaves whose text starts with 'h' — that's leafHi (twice: top-level + inside named)
        val leafValues = ms.map(_.captures(lCap)).collect { case Leaf(v) => v }
        assert(ms.size == 2)
        assert(leafValues.forall(_.startsWith("h")))
      }
      "#match? filters out captures whose text does not match" in {
        val ms = Matcher.matches(q("(Leaf) @l (#match? @l \"^z\")"), branch).toList
        assert(ms.isEmpty)
      }
      "#not-eq? keeps captures whose text differs from the literal" in {
        val ms     = Matcher.matches(q("(Leaf) @l (#not-eq? @l \"hi\")"), branch).toList
        val values = ms.map(_.captures(lCap)).collect { case Leaf(v) => v }
        assert(ms.size == 2)
        assert(values.forall(_ != "hi"))
      }
      "#not-match? keeps captures whose text does not match regex" in {
        val ms     = Matcher.matches(q("(Leaf) @l (#not-match? @l \"^h\")"), branch).toList
        val values = ms.map(_.captures(lCap)).collect { case Leaf(v) => v }
        assert(ms.size == 2)
        assert(values.forall(v => !v.startsWith("h")))
      }
      "#eq? deterministically fails when capture has no text" in {
        val ms = Matcher.matches(q("(Named) @n (#eq? @n \"hi\")"), branch).toList
        assert(ms.isEmpty)
      }
      "#match? deterministically fails when capture has no text" in {
        val ms = Matcher.matches(q("(Named) @n (#match? @n \"^h\")"), branch).toList
        assert(ms.isEmpty)
      }
      "multi-pattern predicate does not produce hidden success for non-captured matches" in {
        val ms              = Matcher.matches(q("(Named) (Leaf) @l (#eq? @l \"bye\")"), branch).toList
        val onlyLeafCapture = ms.forall(_.captures.keySet == Set(lCap))
        val leafValues      = ms.map(_.captures(lCap)).collect { case Leaf(v) => v }
        assert(ms.size == 1)
        assert(onlyLeafCapture)
        assert(leafValues == List("bye"))
      }
    }
    "custom predicates" - {
      "a user-registered predicate is evaluated like the built-ins" in {
        val customRegistry = PredicateRegistry.default.withPredicate(
          PredicateName.Eq,
          new PredicateImpl:
            def evaluate[T](args: PredicateArgs, captures: Map[CaptureName, T])(using
                qt: QueryableTree[T]
            ): Boolean = false // flip #eq? to always-false for this registry
        )
        val ms = Matcher
          .matches(
            q("(Leaf) @l (#eq? @l \"hi\")"),
            leafHi,
            customRegistry
          )
          .toList
        assert(ms.isEmpty) // default would match; the override forces false
      }
    }
  }
