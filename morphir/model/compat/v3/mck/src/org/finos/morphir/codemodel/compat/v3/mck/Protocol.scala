package org.finos.morphir.codemodel.compat.v3.mck

import kyo.*

/**
 * The IR adapter wire protocol of the Morphir Compatibility Kit (finos/morphir `spec/ir/mck/protocol.schema.json`): one
 * JSON object per line on standard input, one answer line per request on standard output, `exit` answered by ending the
 * session. Envelopes are strict: a request with a missing or unknown member is a `protocol_error`.
 */
private[mck] object Protocol:

  /** The contract a `capabilities` request asked for. Without `contractVersion` it is the shipped numeric v1. */
  enum Contract derives CanEqual:
    case V1, V2Draft

    def wire: Structure.Value = this match
      case V1      => Structure.Value.Integer(1)
      case V2Draft => Structure.Value.Str(Contract.V2DraftVersion)

  object Contract:
    val V2DraftVersion = "2.0.0-draft.1"

  final case class Decode(
      version: Long,
      profile: String,
      path: String,
      strip: Boolean,
      node: String,
      input: String
  ) derives CanEqual

  enum Request derives CanEqual:
    case Capabilities(contract: Contract)
    case DecodeNode(decode: Decode)

    /** `readTree` and `writeTree`: valid operations this adapter does not claim. */
    case Unclaimed(op: String)
    case Exit

  /**
   * A failure answer's diagnostic. `cursor` is a JSON pointer into the input; `code` is a kit diagnostic code, or
   * `protocol_error` for a request this adapter cannot take at all.
   */
  final case class Diagnostic(code: String, cursor: Maybe[String], message: String) derives CanEqual

  object Diagnostic:
    val ProtocolErrorCode = "protocol_error"

    def protocolError(message: String): Diagnostic = Diagnostic(ProtocolErrorCode, Present("/"), message)

  /** Why a line was not a request, and the request id to answer with when the envelope's id was valid. */
  final case class Refusal(id: Maybe[Long], diagnostic: Diagnostic) derives CanEqual

  private val Profiles  = Set("json", "yaml", "ion")
  private val PathModes = Set("current", "pinned")

  def parse(line: String): Result[Refusal, (Long, Request)] =
    Json.decode[Structure.Value](line) match
      case Result.Success(Structure.Value.Record(fields)) =>
        val members = fields.toMap
        idOf(members) match
          case Result.Success(id) =>
            requestOf(members - "id") match
              case Result.Success(request) => Result.succeed((id, request))
              case Result.Failure(message) => Result.fail(Refusal(Present(id), Diagnostic.protocolError(message)))
              case Result.Panic(error) => Result.fail(Refusal(Present(id), Diagnostic.protocolError(error.toString)))
          case Result.Failure(message) => Result.fail(Refusal(Absent, Diagnostic.protocolError(message)))
          case Result.Panic(error)     => Result.fail(Refusal(Absent, Diagnostic.protocolError(error.toString)))
      case Result.Success(_) => Result.fail(Refusal(Absent, Diagnostic.protocolError("message must be a JSON object")))
      case Result.Failure(error) => Result.fail(Refusal(Absent, Diagnostic.protocolError(s"not a JSON line: $error")))
      case Result.Panic(error)   =>
        Result.fail(Refusal(Absent, Diagnostic.protocolError(s"not a JSON line: ${error.getMessage}")))

  private def idOf(members: Map[String, Structure.Value]): Result[String, Long] =
    members.get("id") match
      case Some(Structure.Value.Integer(id)) if id >= 1 => Result.succeed(id)
      case Some(Structure.Value.Integer(id))            => Result.fail(s"\"id\" must be at least 1, got $id")
      case _                                            => Result.fail("missing id")

  private def requestOf(members: Map[String, Structure.Value]): Result[String, Request] =
    members.get("op") match
      case Some(Structure.Value.Str("capabilities")) => capabilities(members - "op")
      case Some(Structure.Value.Str("decode"))       => decode(members - "op")
      case Some(Structure.Value.Str("exit"))         => only(members - "op", Set.empty).map(_ => Request.Exit)
      case Some(Structure.Value.Str(op @ ("readTree" | "writeTree"))) => Result.succeed(Request.Unclaimed(op))
      case Some(Structure.Value.Str(other))                           => Result.fail(s"unsupported op \"$other\"")
      case _                                                          => Result.fail("missing \"op\"")

  private def capabilities(members: Map[String, Structure.Value]): Result[String, Request] =
    only(members, Set("contractVersion")).flatMap { _ =>
      members.get("contractVersion") match
        case None                                               => Result.succeed(Request.Capabilities(Contract.V1))
        case Some(Structure.Value.Str(Contract.V2DraftVersion)) =>
          Result.succeed(Request.Capabilities(Contract.V2Draft))
        case Some(other) =>
          Result.fail(
            s"unsupported capabilities contractVersion ${KitJson.write(other)}; expected ${Contract.V2DraftVersion}"
          )
    }

  private def decode(members: Map[String, Structure.Value]): Result[String, Request] =
    val required = Set("version", "profile", "path", "strip", "node", "input")
    only(members, required).flatMap { _ =>
      (
        members.get("version"),
        members.get("profile"),
        members.get("path"),
        members.get("strip"),
        members.get("node"),
        members.get("input")
      ) match
        case (
              Some(Structure.Value.Integer(version)),
              Some(Structure.Value.Str(profile)),
              Some(Structure.Value.Str(path)),
              Some(Structure.Value.Bool(strip)),
              Some(Structure.Value.Str(node)),
              Some(Structure.Value.Str(input))
            ) if Profiles.contains(profile) && PathModes.contains(path) =>
          Result.succeed(Request.DecodeNode(Decode(version, profile, path, strip, node, input)))
        case _ =>
          val missing = required.toList.sorted.filterNot(members.contains)
          if missing.nonEmpty then Result.fail(s"decode is missing ${missing.mkString(", ")}")
          else Result.fail("decode has a member of the wrong type or value")
    }

  /** Rejects any member outside `allowed`; `id` and `op` are already removed. */
  private def only(members: Map[String, Structure.Value], allowed: Set[String]): Result[String, Unit] =
    members.keys.toList.sorted.find(key => !allowed.contains(key)) match
      case Some(extra) => Result.fail(s"unexpected field \"$extra\"")
      case None        => Result.succeed(())

  // Answers -----------------------------------------------------------------------------------------------------------

  def capabilitiesAnswer(id: Long, contract: Contract, capabilities: Capabilities): String =
    KitJson.write(
      record(
        "id"              -> Structure.Value.Integer(id),
        "contractVersion" -> contract.wire,
        "binding"         -> str(capabilities.binding),
        "language"        -> str(capabilities.language),
        "formatVersions"  -> str(capabilities.formatVersions),
        "versions"        -> seq(capabilities.versions.map(Structure.Value.Integer(_))),
        "profiles"        -> seq(capabilities.profiles.map(str)),
        "layouts"         -> seq(capabilities.layouts.map(str)),
        "paths"           -> seq(capabilities.paths.map(str)),
        "nodes"           -> seq(capabilities.nodes.map(str))
      )
    )

  def decodedAnswer(id: Long, kind: String, canonicalJson: String): String =
    KitJson.write(
      record(
        "id"        -> Structure.Value.Integer(id),
        "ok"        -> Structure.Value.Bool(true),
        "kind"      -> str(kind),
        "canonical" -> record("json" -> str(canonicalJson)),
        "warnings"  -> seq(Chunk.empty)
      )
    )

  def failureAnswer(id: Maybe[Long], diagnostic: Diagnostic): String =
    val details = Chunk("code" -> str(diagnostic.code)) ++
      diagnostic.cursor.map(cursor => Chunk("cursor" -> str(cursor))).getOrElse(Chunk.empty) ++
      Chunk("message" -> str(diagnostic.message))
    KitJson.write(
      record(
        "id"         -> id.map(Structure.Value.Integer(_)).getOrElse(Structure.Value.Null),
        "ok"         -> Structure.Value.Bool(false),
        "diagnostic" -> Structure.Value.Record(details)
      )
    )

  /** What the `capabilities` answer claims. */
  final case class Capabilities(
      binding: String,
      language: String,
      formatVersions: String,
      versions: Chunk[Long],
      profiles: Chunk[String],
      layouts: Chunk[String],
      paths: Chunk[String],
      nodes: Chunk[String]
  )

  private def str(text: String): Structure.Value                          = Structure.Value.Str(text)
  private def seq(items: Chunk[Structure.Value]): Structure.Value         = Structure.Value.Sequence(items)
  private def record(fields: (String, Structure.Value)*): Structure.Value = Structure.Value.Record(Chunk.from(fields))
end Protocol
