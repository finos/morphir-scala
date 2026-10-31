# morphir

This directory holds the Morphir library modules, one sub-directory for each module family. It has no sources of its
own.

## The classic library was removed

Until 0.5.0-M12, this directory also held the classic ZIO-based Morphir library. Version 0.5.0-M13 removed it and the
artifacts that depended on it. Versions already on Maven Central stay there. Use the Kyo-based modules instead:

| Removed artifact | Use instead |
| --- | --- |
| `morphir` (`morphir_3`, `morphir_sjs1_3`) | `morphir-model` (code model and datamodel), `morphir-naming`, and `morphir-model-compat-v3` |
| `morphir-interop-zio-json` | `morphir-model` for v4 JSON; `morphir-model-compat-v3` for v3 `morphir-ir.json` |
| `morphir-interop-borer` | the datamodel and Kyo Schema codecs in `morphir-model` |
| `morphir-model-lowering` | `V3Reader` in `morphir-model-compat-v3` |
| `morphir-runtime-classic` | no replacement; Scala evaluation of Morphir IR was removed |
| `morphir-tools` | `kyo-system` for files and processes; the `mill-morphir-elm` plugin or the Morphir CLI to run morphir-elm |
| `morphir-tests` | no replacement |

`V3Reader` reads the v3 `morphir-ir.json` that morphir-elm writes. The `mck-ir` CI job holds it to the Morphir
Compatibility Kit from finos/morphir. See [v3 IR Reading](../kb/bundles/morphir/morphir-scala/v3-ir-reading.md).
