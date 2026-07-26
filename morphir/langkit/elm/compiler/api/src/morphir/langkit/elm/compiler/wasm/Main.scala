package morphir.langkit.elm.compiler.wasm

/**
 * Reachable module initializer for the Scala.js Wasm backend.
 *
 * `compiler.api.wasm` is consumed as an implementation detail behind a JS facade, but the Scala.js linker still needs a
 * main entrypoint to emit the `main.js` loader and `main.wasm` bundle. The initializer is intentionally a no-op:
 * runtime behavior stays in shared compiler APIs, while this object exists only to make the Wasm artifact loadable and
 * discoverable.
 */
object Main:
  def main(args: Array[String]): Unit = ()
