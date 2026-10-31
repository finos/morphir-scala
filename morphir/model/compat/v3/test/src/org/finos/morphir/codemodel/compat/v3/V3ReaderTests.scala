package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.test.*

import V3ReadFixtures.*

/**
 * The document envelope and its `formatVersion`. The scalar cases are finos/morphir
 * `docs/spec/ir/fixtures/format-version-conformance.json`, judged against this reader's table, `[3.0.0,3.2.0)`, rather
 * than the corpus's reference table: a version 4 release is `version_mismatch` here, not supported.
 */
class V3ReaderTests extends Test[Any]:

  private val library = """["Library",[["p"]],[],{"modules":[]}]"""

  private def document(formatVersion: String): String = s"""{"formatVersion":$formatVersion,"distribution":$library}"""

  private def codeOf(formatVersion: String): Maybe[String] =
    failure(V3Reader.decodeIRDocument(document(formatVersion))).map(_.code)

  private def releaseOf(formatVersion: String): Maybe[String] =
    success(V3Reader.decodeIRDocument(document(formatVersion))).map(_.release.toString)

  "the formatVersion envelope" - {
    "reads the integer 3 as release 3.0.0" in {
      assert(releaseOf("3") == Present("3.0.0"))
      assert(success(V3Reader.decodeIRDocument(document("3"))).map(_.release) == Present(V3Release.Baseline))
    }
    "reads the release strings of the supported minors" in {
      assert(releaseOf("\"3.0.0\"") == Present("3.0.0"))
      assert(releaseOf("\"3.0.1\"") == Present("3.0.1"))
      assert(releaseOf("\"3.1.0\"") == Present("3.1.0"))
      assert(releaseOf("\"3.1.4294967295\"") == Present("3.1.4294967295"))
    }
    "reads formatVersion after the distribution too" in
      assert(
        success(V3Reader.decodeIRDocument(s"""{"distribution":$library,"formatVersion":"3.1.0"}""")).map(
          _.release.toString
        ) == Present("3.1.0")
      )
    "rejects the historical integers 1 and 2 as an unsupported major, naming the version" in {
      val error = failure(V3Reader.decodeIRFile(document("1")))
      assert(error == Present(V3ReadError.UnsupportedFormatVersionMajor("1")))
      assert(error.map(_.code) == Present("unsupported_format_version_major"))
      assert(error.map(_.location) == Present("$.formatVersion"))
      assert(error.exists(_.message.contains("formatVersion 1")))
      assert(codeOf("2") == Present("unsupported_format_version_major"))
    }
    "rejects a later version 3 minor" in {
      assert(failure(V3Reader.decodeIRFile(document("\"3.2.0\""))) ==
        Present(V3ReadError.UnsupportedFormatVersionMinor("\"3.2.0\"")))
      assert(codeOf("\"3.4294967295.0\"") == Present("unsupported_format_version_minor"))
    }
    "rejects a version 4 release as a document for another reader" in {
      assert(codeOf("4") == Present("version_mismatch"))
      assert(codeOf("\"4.0.0\"") == Present("version_mismatch"))
      assert(codeOf("\"4.0.1\"") == Present("version_mismatch"))
      assert(codeOf("\"4.1.0\"") == Present("version_mismatch"))
    }
    "rejects a family from 5 up as an unsupported major" in {
      assert(codeOf("5") == Present("unsupported_format_version_major"))
      assert(codeOf("\"5.0.0\"") == Present("unsupported_format_version_major"))
      assert(codeOf("\"5.2.7\"") == Present("unsupported_format_version_major"))
      assert(codeOf("4294967295") == Present("unsupported_format_version_major"))
      assert(codeOf("\"4294967295.0.0\"") == Present("unsupported_format_version_major"))
    }
    "rejects a component above 4294967295 as out of range" in {
      assert(codeOf("4294967296") == Present("format_version_out_of_range"))
      assert(codeOf("\"4294967296.0.0\"") == Present("format_version_out_of_range"))
      assert(codeOf("\"3.4294967296.0\"") == Present("format_version_out_of_range"))
      assert(codeOf("\"4.0.4294967296\"") == Present("format_version_out_of_range"))
    }
    "rejects what is not the exact release grammar as invalid syntax" in {
      val invalid = List(
        "0",
        "\"1.0.0\"",
        "\"2.0.0\"",
        "\"3.0\"",
        "\"3.0.0.0\"",
        "\"3.+1.0\"",
        "\"3.-1.0\"",
        "\" 3.0.0\"",
        "\"3.01.0\"",
        "\"4.0.01\"",
        "\"3.0.0-alpha\"",
        "\"4.0.0+build\""
      )
      assert(invalid.map(codeOf).forall(_ == Present("invalid_format_version_syntax")))
    }
    "rejects a value that is not a string or unsigned integer as the wrong type" in {
      val invalid = List("-1", "3.5", "true", "null", "[3]", """{"major":3}""")
      assert(invalid.map(codeOf).forall(_ == Present("invalid_format_version_type")))
    }
    "requires a formatVersion member" in {
      val error = failure(V3Reader.decodeIRFile(s"""{"distribution":$library}"""))
      assert(error == Present(V3ReadError.MissingFormatVersion))
      assert(error.map(_.code) == Present("missing_format_version"))
      assert(error.map(_.location) == Present("$"))
    }
    "rejects a duplicate formatVersion member" in {
      val error = failure(V3Reader.decodeIRFile(s"""{"formatVersion":3,"formatVersion":3,"distribution":$library}"""))
      assert(error.map(_.code) == Present("duplicate_format_version"))
    }
    "checks formatVersion before anything else" in
      // document-tree-0016: a v4 node file offered as a v3 document has no distribution member at all.
      assert(
        failure(V3Reader.decodeIRFile("""{"formatVersion":"4.0.0","name":"user"}""")) ==
          Present(V3ReadError.VersionMismatch("\"4.0.0\""))
      )
    "requires a distribution member" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3}"""))
      assert(error == Present(V3ReadError.WrongShape("$.distribution", "a distribution member")))
    }
    "requires an object at the root" in {
      val error = failure(V3Reader.decodeIRFile("""[3]"""))
      assert(error.map(_.code) == Present("invalid_type"))
      assert(error.map(_.location) == Present("$"))
    }
  }

  "a formatVersion on its own" - {
    "reads a supported release and spells it canonically" in {
      assert(success(V3Reader.decodeFormatVersion("3")).map(_.canonicalJson) == Present("3"))
      assert(success(V3Reader.decodeFormatVersion("\"3.0.0\"")).map(_.canonicalJson) == Present("3"))
      assert(success(V3Reader.decodeFormatVersion("\"3.1.0\"")).map(_.canonicalJson) == Present("\"3.1.0\""))
    }
    "refuses an unsupported one with the same codes" in
      assert(failure(V3Reader.decodeFormatVersion("\"3.2.0\"")).map(_.code) ==
        Present("unsupported_format_version_minor"))
  }

  "malformed input" - {
    "reports truncated JSON as invalid_json" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":["Libr"""))
      assert(error.map(_.code) == Present("invalid_json"))
    }
    "reports an unknown distribution tag with its path, as invalid_type" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":["Archive",[],[],{}]}"""))
      assert(error == Present(V3ReadError.UnknownTag("$.distribution", "Archive")))
      assert(error.map(_.code) == Present("invalid_type"))
    }
    "reports a distribution that is not a tagged array" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":{}}"""))
      assert(error == Present(V3ReadError.WrongShape("$.distribution", "a tagged array")))
    }
  }
end V3ReaderTests
