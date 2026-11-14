package morphir.spikes.wasmhost

import kyo.test.*

class AdmissionTests extends kyo.test.Test[Any]:
  private def refused(body: => Any): Boolean =
    try
      val _ = body
      false
    catch case _: IllegalArgumentException => true

  "admission rejects components and malformed core headers" in
    assert(refused(WasmSurface.inspect(Array[Byte](0, 97, 115, 109, 13, 0, 1, 0))))

  "admission bounds section and vector lengths before allocation" in {
    val header = Array[Byte](0, 97, 115, 109, 1, 0, 0, 0)
    assert(refused(WasmSurface.inspect(header ++ Array[Byte](1, -1, -1, -1, -1, 127))))
    assert(refused(WasmSurface.inspect(header ++ Array[Byte](1, 1, -1))))
  }

  "MEP rejects malformed UTF-8 rather than replacing bytes" in {
    val rejected = try
      val _ = MepWorker.utf8(Array[Byte](-61, 40))
      false
    catch case _: java.nio.charset.CharacterCodingException => true
    assert(rejected)
  }

  "JSON-RPC uint64 identity stays exact and is distinct from nested ids" in {
    assert(JsonIdentity.extract("{\"params\":{\"id\":0},\"id\":18446744073709551615}") !=
      JsonIdentity.extract("{\"id\":18446744073709551614}"))
    assert(JsonIdentity.extract("{\"id\":\"日本語 🚀\"}") == JsonIdentity.extract("{\"id\":\"日本語 🚀\"}"))
    assert(JsonIdentity.extract("{\"id\":1e2}") == JsonIdentity.extract("{\"id\":100}"))
  }
