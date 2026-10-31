package org.finos.morphir.codemodel.compat.v3.mck

import kyo.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.codemodel.compat.v3.*

import scala.annotation.tailrec

/** What the adapter does with one line of input. */
enum Reply derives CanEqual:
  /** Write this line to standard output. */
  case Answer(line: String)

  /** A blank line: nothing to answer. */
  case Silent

  /** `exit`: end the session without answering. */
  case Exit

/**
 * The Morphir Compatibility Kit IR adapter for the v3 reader. It claims format version 3 only, the JSON profile and the
 * single-document layout, and answers `decode` by reading the input with [[V3Reader]] and writing the node back through
 * [[V3WireProjection]] in the kit's canonical JSON spelling.
 */
object V3MckAdapter:
  import Protocol.*

  /** The node kinds the v3 reader decodes, spelled as the kit names them. */
  val nodes: Chunk[String] = Chunk(
    "Name",
    "Path",
    "FQName",
    "Type",
    "Literal",
    "Pattern",
    "Value",
    "TypeSpecification",
    "TypeDefinition",
    "ValueSpecification",
    "ValueDefinition",
    "ModuleDefinition",
    "ModuleSpecification",
    "IRFile",
    "Distribution",
    "FormatVersion"
  )

  /**
   * The v3 releases the reader reads, as a canonical support table (finos/morphir `docs/spec/ir/format-version.md`).
   */
  val formatVersions: String = V3Reader.SupportTable

  private val capabilities = Capabilities(
    binding = "morphir-scala",
    language = "scala",
    formatVersions = formatVersions,
    versions = Chunk(3L),
    profiles = Chunk("json"),
    layouts = Chunk("single"),
    paths = Chunk("current"),
    nodes = nodes
  )

  /** Code for a node the reader read but the v3 writer cannot spell, such as a `Specs` distribution. */
  val UnwritableNodeCode = "unwritable_node"

  def respond(line: String): Reply =
    if line.trim.isEmpty then Reply.Silent
    else
      Protocol.parse(line) match
        case Result.Success((id, request)) => handle(id, request)
        case Result.Failure(refusal)       => Reply.Answer(failureAnswer(refusal.id, refusal.diagnostic))
        case Result.Panic(error)           =>
          Reply.Answer(failureAnswer(Absent, Diagnostic.protocolError(s"unreadable request: $error")))

  /** Answers each line of `lines` until `exit` or the end of input, writing every answer through `write`. */
  def serve(lines: Iterator[String])(write: String => Unit): Unit =
    @tailrec def loop(): Unit =
      if lines.hasNext then
        respond(lines.next()) match
          case Reply.Answer(answer) =>
            write(answer)
            loop()
          case Reply.Silent => loop()
          case Reply.Exit   => ()
    loop()

  private def handle(id: Long, request: Request): Reply =
    request match
      case Request.Capabilities(contract) => Reply.Answer(capabilitiesAnswer(id, contract, capabilities))
      case Request.Exit                   => Reply.Exit
      case Request.Unclaimed(op)          =>
        Reply.Answer(
          failureAnswer(Present(id), Diagnostic.protocolError(s"op \"$op\" is not in this adapter's capabilities"))
        )
      case Request.DecodeNode(decode) => Reply.Answer(decodeAnswer(id, decode))

  private def decodeAnswer(id: Long, decode: Decode): String =
    if decode.version != 3 then
      failureAnswer(
        Present(id),
        Diagnostic.protocolError(s"this adapter reads IR version 3 only, not ${decode.version}")
      )
    else if decode.profile != "json" then
      failureAnswer(
        Present(id),
        Diagnostic.protocolError(s"this adapter reads the json profile only, not ${decode.profile}")
      )
    else if !nodes.contains(decode.node) then
      failureAnswer(Present(id), Diagnostic.protocolError(s"node ${decode.node} is not in this adapter's capabilities"))
    else
      val attributes = if decode.strip then V3AttributeMode.Erased else V3AttributeMode.InferredTypes
      read(decode.node, decode.input, attributes) match
        case Result.Success((kind, tree)) => decodedAnswer(id, kind, KitJson.write(tree) + "\n")
        case Result.Failure(diagnostic)   => failureAnswer(Present(id), diagnostic)
        case Result.Panic(error)          =>
          failureAnswer(Present(id), Diagnostic(UnwritableNodeCode, Present("/"), s"adapter failure: $error"))

  private type Read = Result[Diagnostic, (String, Structure.Value)]

  private def read(node: String, input: String, attributes: V3AttributeMode): Read =
    import V3WireProjection.Nodes
    node match
      case "Name"   => answer(V3Reader.decodeName(input))(_ => "Name", name => Result.succeed(Nodes.name(name)))
      case "Path"   => answer(V3Reader.decodePath(input))(_ => "Path", path => Result.succeed(Nodes.path(path)))
      case "FQName" =>
        answer(V3Reader.decodeFQName(input))(_ => "FQName", fqName => Result.succeed(Nodes.fqName(fqName)))
      case "Type"               => answer(V3Reader.decodeType(input))(variant, Nodes.tpe)
      case "Literal"            => answer(V3Reader.decodeLiteral(input))(literalKind, Nodes.literal)
      case "Pattern"            => answer(V3Reader.decodePattern(input))(variant, Nodes.pattern(_, attributes))
      case "Value"              => answer(V3Reader.decodeValue(input))(variant, Nodes.value(_, attributes))
      case "TypeSpecification"  => answer(V3Reader.decodeTypeSpecification(input))(variant, Nodes.typeSpecification)
      case "TypeDefinition"     => answer(V3Reader.decodeTypeDefinition(input))(variant, Nodes.typeDefinition)
      case "ValueSpecification" =>
        answer(V3Reader.decodeValueSpecification(input))(_ => "ValueSpecification", Nodes.valueSpecification)
      case "ValueDefinition" =>
        answer(V3Reader.decodeValueDefinition(input))(_ => "ValueDefinition", Nodes.valueDefinition(_, attributes))
      case "ModuleDefinition" =>
        answer(V3Reader.decodeModuleDefinition(input))(_ => "ModuleDefinition", Nodes.moduleDefinition(_, attributes))
      case "ModuleSpecification" =>
        answer(V3Reader.decodeModuleSpecification(input))(_ => "ModuleSpecification", Nodes.moduleSpecification)
      case "FormatVersion" =>
        answer(V3Reader.decodeFormatVersion(input))(
          _ => "FormatVersion",
          release => Result.succeed(V3WireProjection.formatVersion(release))
        )
      // The kit names a whole document `Distribution` as well as `IRFile`; both carry the formatVersion envelope.
      case _ => answer(V3Reader.decodeIRDocument(input))(distributionKind, document(_, attributes))

  private def answer[A](read: V3Reader.Read[A])(
      kind: A => String,
      project: A => Result[V3ProjectionError, Structure.Value]
  ): Read =
    read match
      case Result.Success(node) =>
        project(node) match
          case Result.Success(tree)    => Result.succeed((kind(node), tree))
          case Result.Failure(failure) => Result.fail(unwritable(failure))
          case Result.Panic(error)     => Result.fail(Diagnostic(UnwritableNodeCode, Present("/"), error.toString))
      case Result.Failure(error) => Result.fail(Diagnostic(error.code, cursor(error.location), error.message))
      case Result.Panic(error)   => Result.fail(Diagnostic(UnwritableNodeCode, Present("/"), error.toString))

  /** A `Bundle` reads as one library per package, which has no single-library v3 spelling to write back. */
  private def document(
      read: V3Document,
      attributes: V3AttributeMode
  ): Result[V3ProjectionError, Structure.Value] =
    val distributions = read.distributions
    if distributions.size == 1 then V3WireProjection.project(distributions.head, attributes, read.release)
    else Result.fail(V3ProjectionError.UnsupportedDistribution(s"Bundle of ${distributions.size} packages"))

  private def distributionKind(read: V3Document): String =
    if read.distributions.size == 1 then variant(read.distributions.head) else "Bundle"

  private def literalKind(literal: cm.Literal): String =
    literal match
      case _: cm.Literal.IntegerLiteral => "WholeNumberLiteral"
      case other                        => variant(other)

  /** The v3 tag of a node: its case name, which the code model shares with v3 for every kind but literals. */
  private def variant(node: Product): String = node.productPrefix

  private def unwritable(failure: V3ProjectionError): Diagnostic =
    failure match
      case V3ProjectionError.UnsupportedDistribution(kind) =>
        Diagnostic(UnwritableNodeCode, Present("/"), s"a $kind distribution has no v3 spelling this writer produces")
      case V3ProjectionError.UnsupportedFeature(path, feature) =>
        Diagnostic(UnwritableNodeCode, cursor(path), s"$path: $feature has no v3 spelling")
      case V3ProjectionError.InvalidModel(path, details) =>
        Diagnostic(UnwritableNodeCode, cursor(path), s"$path: $details")

  /**
   * The JSON pointer for a reader location such as `$.distribution[1]`, which is `/distribution/1`. A location that is
   * not rooted at `$` has no pointer.
   */
  private[mck] def cursor(location: String): Maybe[String] =
    if !location.startsWith("$") then Absent
    else
      @tailrec def loop(rest: List[Char], segment: String, segments: List[String]): List[String] =
        rest match
          case Nil         => (segment :: segments).reverse
          case '.' :: tail => loop(tail, "", segment :: segments)
          case '[' :: tail =>
            val (index, after) = tail.span(_ != ']')
            loop(after.drop(1), "", index.mkString :: segment :: segments)
          case char :: tail => loop(tail, segment + char, segments)
      val segments = loop(location.drop(1).toList, "", Nil).filter(_.nonEmpty)
      Present(segments.map(_.replace("~", "~0").replace("/", "~1")).mkString("/", "/", ""))
end V3MckAdapter
