package org.finos.morphir.codemodel.compat.v3.mck

import kyo.*
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.codemodel.compat.v3.*

import scala.collection.mutable.ListBuffer

class V3MckAdapterTests extends Test[Any]:

  private def answer(line: String): String =
    V3MckAdapter.respond(line) match
      case Reply.Answer(text) => text
      case other              => throw new AssertionError(s"expected an answer to $line, got $other")

  private val nodes =
    """["Name", "Path", "FQName", "Type", "Literal", "Pattern", "Value", "TypeSpecification", "TypeDefinition", """ +
      """"ValueSpecification", "ValueDefinition", "ModuleDefinition", "ModuleSpecification", "IRFile", "Distribution"]"""

  private def capabilities(id: Int, contract: String): String =
    s"""{ "id": $id, "contractVersion": $contract, "binding": "morphir-scala", "language": "scala", """ +
      s""""formatVersions": "[3.0.0,3.1.0)", "versions": [3], "profiles": ["json"], "layouts": ["single"], """ +
      s""""paths": ["current"], "nodes": $nodes }"""

  private def protocolError(id: String, message: String): String =
    s"""{ "id": $id, "ok": false, "diagnostic": { "code": "protocol_error", "cursor": "/", "message": "$message" } }"""

  private def decode(id: Int, node: String, input: String, version: Int = 3, profile: String = "json"): String =
    val escaped = input.replace("\\", "\\\\").replace("\"", "\\\"")
    s"""{"id":$id,"op":"decode","version":$version,"profile":"$profile","path":"current","strip":true,""" +
      s""""node":"$node","input":"$escaped"}"""

  "capabilities" - {
    "answers an unversioned request with contract 1, claiming version 3, json and single only" in
      assert(answer("""{"id":1,"op":"capabilities"}""") == capabilities(1, "1"))
    "answers the v2 draft request with the draft contract and the same claims" in
      assert(
        answer("""{"id":1,"op":"capabilities","contractVersion":"2.0.0-draft.1"}""") ==
          capabilities(1, "\"2.0.0-draft.1\"")
      )
    "refuses another contract version with protocol_error, so the runner falls back to v1" in
      assert(
        answer("""{"id":1,"op":"capabilities","contractVersion":"3.0.0"}""") ==
          protocolError("1", """unsupported capabilities contractVersion \"3.0.0\"; expected 2.0.0-draft.1""")
      )
  }

  "the envelope" - {
    "answers a line that is not JSON with a null id" in
      assert(answer("{nope").startsWith("""{ "id": null, "ok": false, "diagnostic": { "code": "protocol_error""""))
    "answers a message that is not an object" in
      assert(answer("[1]") == protocolError("null", "message must be a JSON object"))
    "answers a missing id with a null id" in
      assert(answer("""{"op":"capabilities"}""") == protocolError("null", "missing id"))
    "answers an id below 1 with a null id" in
      assert(answer("""{"id":0,"op":"exit"}""") == protocolError("null", """\"id\" must be at least 1, got 0"""))
    "names the id when the op is unknown" in
      assert(answer("""{"id":4,"op":"translate"}""") == protocolError("4", """unsupported op \"translate\""""))
    "rejects an unknown member" in
      assert(answer("""{"id":2,"op":"exit","now":true}""") == protocolError("2", """unexpected field \"now\""""))
    "ignores a blank line" in
      assert(V3MckAdapter.respond("   ") == Reply.Silent)
  }

  "unclaimed operations" - {
    "answer readTree with the protocol error response" in {
      val line = """{"id":3,"op":"readTree","version":3,"profile":"json","path":"current","strip":true,""" +
        """"node":"IRFile","files":[]}"""
      assert(answer(line) == protocolError("3", """op \"readTree\" is not in this adapter's capabilities"""))
    }
    "answer writeTree with the protocol error response" in {
      val line = """{"id":3,"op":"writeTree","version":3,"path":"current",""" +
        """"policy":{"profile":"json","pathBudget":4000},"input":"{}"}"""
      assert(answer(line) == protocolError("3", """op \"writeTree\" is not in this adapter's capabilities"""))
    }
  }

  "decode" - {
    "refuses a version other than 3" in
      assert(
        answer(decode(2, "Value", "1", version = 4)) ==
          protocolError("2", "this adapter reads IR version 3 only, not 4")
      )
    "refuses a profile other than json" in
      assert(
        answer(decode(2, "Value", "x", profile = "yaml")) ==
          protocolError("2", "this adapter reads the json profile only, not yaml")
      )
    "refuses a node kind it does not claim" in
      assert(
        answer(decode(2, "DistributionManifestFile", "{}")) ==
          protocolError("2", "node DistributionManifestFile is not in this adapter's capabilities")
      )
    "rejects a missing member" in
      assert(
        answer("""{"id":2,"op":"decode","version":3,"profile":"json","path":"current","node":"Value","input":"1"}""")
          == protocolError("2", "decode is missing strip")
      )
    "rejects an older format version with the reader's diagnostic code and cursor" in {
      val line = decode(5, "IRFile", """{"formatVersion":1,"distribution":["Library",[],[],{}]}""")
      assert(
        answer(line) ==
          """{ "id": 5, "ok": false, "diagnostic": { "code": "unsupported_format_version", "cursor": "/formatVersion", """ +
          """"message": "formatVersion 1 is not supported; only 3 is. Regenerate the IR with current morphir-elm." } }"""
      )
    }
    "reads a whole document for the Distribution node too" in {
      val line = decode(6, "Distribution", """{"formatVersion":2,"distribution":["Library",[],[],{}]}""")
      assert(answer(line).contains(""""code": "unsupported_format_version""""))
    }
    "reports malformed input as invalid_json" in
      assert(answer(decode(7, "IRFile", """{"formatVersion":3,""")).contains(""""code": "invalid_json""""))
  }

  "a session" - {
    "answers each request in order and stops at exit" in {
      val written = ListBuffer.empty[String]
      V3MckAdapter.serve(
        Iterator(
          """{"id":1,"op":"capabilities"}""",
          "",
          """{"id":2,"op":"exit"}""",
          """{"id":3,"op":"capabilities"}"""
        )
      )(written += _)
      assert(written.toList == List(capabilities(1, "1")))
    }
  }

  "canonical JSON" - {
    val literal = cm.Literal.IntegerLiteral(BigInt(42))
    val unit    = cm.Type.Unit(cm.TypeAttributes.empty)
    val typed   = cm.ValueAttributes.empty.copy(inferredType = Some(unit))

    def written(projection: Result[V3ProjectionError, Structure.Value]): String =
      projection match
        case Result.Success(tree) => KitJson.write(tree)
        case other                => throw new AssertionError(s"projection failed: $other")

    "spells a v3 literal value the way the kit does (versions-0001)" in {
      val value = cm.Expr.Literal(cm.ValueAttributes.empty, literal)
      assert(
        written(V3WireProjection.Nodes.value(value, V3AttributeMode.Erased)) ==
          """["Literal", {}, ["WholeNumberLiteral", 42]]"""
      )
    }
    "keeps a typed value's type in its attribute slot" in {
      val value = cm.Expr.Literal(typed, literal)
      assert(
        written(V3WireProjection.Nodes.value(value, V3AttributeMode.InferredTypes)) ==
          """["Literal", ["Unit", {}], ["WholeNumberLiteral", 42]]"""
      )
    }
    "empties the attribute slot when the runner asks to strip" in {
      val value = cm.Expr.Literal(typed, literal)
      assert(
        written(V3WireProjection.Nodes.value(value, V3AttributeMode.Erased)) ==
          """["Literal", {}, ["WholeNumberLiteral", 42]]"""
      )
    }
    "pads a non-empty object and separates members with a comma and a space" in {
      val tree = Structure.Value.Record(
        Chunk("a" -> Structure.Value.Integer(1), "b" -> Structure.Value.Record(Chunk.empty))
      )
      assert(KitJson.write(tree) == """{ "a": 1, "b": {} }""")
    }
    "escapes what JSON requires in a string and nothing more" in
      assert(KitJson.write(Structure.Value.Str("a\"b\\c\nd\u0001é/")) == "\"a\\\"b\\\\c\\nd\\u0001é/\"")
  }

  "cursor" - {
    "turns a reader location into a JSON pointer" in {
      assert(V3MckAdapter.cursor("$") == Present("/"))
      assert(V3MckAdapter.cursor("$.formatVersion") == Present("/formatVersion"))
      assert(V3MckAdapter.cursor("$.distribution[3].modules") == Present("/distribution/3/modules"))
      assert(V3MckAdapter.cursor("distribution.definition") == Absent)
    }
  }
end V3MckAdapterTests
