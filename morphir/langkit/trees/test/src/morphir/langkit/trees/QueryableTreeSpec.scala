package morphir.langkit.trees

import kyo.test.*

import morphir.langkit.trees.ToyTree.*

class QueryableTreeSpec extends Test[Any]:

  private val qt: QueryableTree[ToyTree] = summon[QueryableTree[ToyTree]]

  private val leafType: NodeTypeName   = NodeTypeName.make("Leaf").toOption.get
  private val branchType: NodeTypeName = NodeTypeName.make("Branch").toOption.get
  private val namedType: NodeTypeName  = NodeTypeName.make("Named").toOption.get

  private val nameField: FieldName = FieldName.make("name").toOption.get
  private val bodyField: FieldName = FieldName.make("body").toOption.get

  private val leaf: ToyTree              = Leaf("hello")
  private val anotherLeaf: ToyTree       = Leaf("world")
  private val branch: ToyTree            = Branch(Seq(leaf, anotherLeaf))
  private val named: ToyTree             = Named(name = leaf, body = anotherLeaf)
  private val allVariants: List[ToyTree] = List(leaf, anotherLeaf, branch, named)

  "QueryableTree[ToyTree]" - {
    "nodeType" - {
      "is non-empty for every variant" in
        assert(allVariants.forall(t => NodeTypeName.unwrap(qt.nodeType(t)).nonEmpty))
      "uses the simple class name" in {
        assert(qt.nodeType(leaf) == leafType)
        assert(qt.nodeType(branch) == branchType)
        assert(qt.nodeType(named) == namedType)
      }
    }
    "children" - {
      "Leaf has no children" in
        assert(qt.children(leaf).isEmpty)
      "Branch enumerates its items in order" in
        assert(qt.children(branch) == Seq(leaf, anotherLeaf))
      "Named exposes name then body as children" in
        assert(qt.children(named) == Seq(leaf, anotherLeaf))
      "children is stable under repeated invocation" in {
        val first  = qt.children(branch)
        val second = qt.children(branch)
        assert(first == second)
      }
    }
    "fields" - {
      "Leaf and Branch expose no fields" in {
        assert(qt.fields(leaf).isEmpty)
        assert(qt.fields(branch).isEmpty)
      }
      "Named exposes name and body keys" in {
        val fs = qt.fields(named)
        assert(fs.keySet == Set(nameField, bodyField))
        assert(fs(nameField) == Seq(leaf))
        assert(fs(bodyField) == Seq(anotherLeaf))
      }
      "all field values appear among children" in {
        val fieldValues = qt.fields(named).values.flatten.toSet
        val kids        = qt.children(named).toSet
        assert(fieldValues.subsetOf(kids))
      }
    }
    "text" - {
      "Leaf returns Some(value)" in
        assert(qt.text(leaf).contains("hello"))
      "compound nodes return None" in {
        assert(qt.text(branch).isEmpty)
        assert(qt.text(named).isEmpty)
      }
    }
  }
