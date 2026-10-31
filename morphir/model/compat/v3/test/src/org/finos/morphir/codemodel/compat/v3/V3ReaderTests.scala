package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.test.*

class V3ReaderTests extends Test[Any]:

  private def failure[A](read: V3Reader.Read[A]): Maybe[V3ReadError] =
    read match
      case Result.Failure(error) => Present(error)
      case _                     => Absent

  "the formatVersion envelope" - {
    "rejects format version 1, naming the version" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":1,"distribution":["Library",[],[],{}]}"""))
      assert(error == Present(V3ReadError.UnsupportedFormatVersion("1")))
      assert(error.map(_.code) == Present("unsupported_format_version"))
      assert(error.exists(_.message.contains("formatVersion 1")))
    }
    "rejects format version 2, naming the version" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":2,"distribution":["Library",[],[],{}]}"""))
      assert(error == Present(V3ReadError.UnsupportedFormatVersion("2")))
    }
    "requires a formatVersion member" in {
      val error = failure(V3Reader.decodeIRFile("""{"distribution":["Library",[],[],{}]}"""))
      assert(error == Present(V3ReadError.WrongShape("$.formatVersion", "a formatVersion member")))
    }
    "requires a distribution member" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3}"""))
      assert(error == Present(V3ReadError.WrongShape("$.distribution", "a distribution member")))
    }
    "requires an object at the root" in {
      val error = failure(V3Reader.decodeIRFile("""[3]"""))
      assert(error.map(_.code) == Present("invalid_shape"))
      assert(error.map(_.location) == Present("$"))
    }
  }

  "malformed input" - {
    "reports truncated JSON as invalid_json" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":["Libr"""))
      assert(error.map(_.code) == Present("invalid_json"))
    }
    "reports an unknown distribution tag with its path" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":["Archive",[],[],{}]}"""))
      assert(error == Present(V3ReadError.UnknownTag("$.distribution", "Archive")))
      assert(error.map(_.code) == Present("unknown_tag"))
    }
    "reports a distribution that is not a tagged array" in {
      val error = failure(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":{}}"""))
      assert(error == Present(V3ReadError.WrongShape("$.distribution", "a tagged array")))
    }
  }
end V3ReaderTests
