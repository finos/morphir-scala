package morphir.spikes.wasmhost.tools

import java.nio.file.Path
import morphir.spikes.wasmhost.*

object Inspect:
  def main(args: Array[String]): Unit =
    val guests = os.Path(Path.of(args(0)).toRealPath())
    val output = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val receipts = Seq("sdk", "python").map { kind =>
      val file    = guests / s"$kind.wasm"
      val surface = WasmSurface.inspect(os.read.bytes(file))
      ujson.Obj(
        "guest"                      -> kind,
        "sha256"                     -> Distribution.hash(file),
        "bytes"                      -> os.size(file),
        "unboundedMemoryDeclaration" -> surface.unboundedMemory,
        "imports"                    -> ujson.Arr.from(surface.imports.map(entry =>
          ujson.Obj(
            "module"    -> entry.module,
            "name"      -> entry.name,
            "kind"      -> entry.kind,
            "typeIndex" -> entry.typeIndex
          )
        )),
        "functionTypes" -> ujson.Arr.from(surface.types.map(signature =>
          ujson.Obj(
            "params"  -> ujson.Arr.from(signature.params),
            "results" -> ujson.Arr.from(signature.results)
          )
        )),
        "exports" -> ujson.Arr.from(surface.exports)
      )
    }
    os.write(output / "surfaces.json", ujson.Arr.from(receipts).render(indent = 2))
