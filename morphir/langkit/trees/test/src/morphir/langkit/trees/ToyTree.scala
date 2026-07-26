package morphir.langkit.trees

/**
 * Minimal tree fixture used by the QueryableTree test suite.
 *
 * Covers the three shapes callers need to exercise: leaves with text, anonymous containers, and nodes with named
 * fields.
 */
sealed trait ToyTree derives CanEqual

object ToyTree:

  final case class Leaf(value: String)                 extends ToyTree derives CanEqual
  final case class Branch(items: Seq[ToyTree])         extends ToyTree derives CanEqual
  final case class Named(name: ToyTree, body: ToyTree) extends ToyTree derives CanEqual

  private val LeafName: NodeTypeName   = NodeTypeName.make("Leaf").toOption.get
  private val BranchName: NodeTypeName = NodeTypeName.make("Branch").toOption.get
  private val NamedName: NodeTypeName  = NodeTypeName.make("Named").toOption.get

  given QueryableTree[ToyTree] with

    def nodeType(t: ToyTree): NodeTypeName = t match
      case _: Leaf   => LeafName
      case _: Branch => BranchName
      case _: Named  => NamedName

    def children(t: ToyTree): Seq[ToyTree] = t match
      case _: Leaf           => Seq.empty
      case Branch(items)     => items
      case Named(name, body) => Seq(name, body)

    private val NameField: FieldName = FieldName.unsafeMake("name")
    private val BodyField: FieldName = FieldName.unsafeMake("body")

    def fields(t: ToyTree): Map[FieldName, Seq[ToyTree]] = t match
      case _: Leaf | _: Branch => Map.empty
      case Named(name, body)   => Map(NameField -> Seq(name), BodyField -> Seq(body))

    def text(t: ToyTree): Option[String] = t match
      case Leaf(v) => Some(v)
      case _       => None
