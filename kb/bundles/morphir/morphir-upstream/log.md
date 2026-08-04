# Log

## 2026-08-02

* **Creation**: Bundle created as the working location for the Morphir IR specification, design documents and
  schemas. Imported 82 files from `finos/morphir` at `4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc` — 40 markdown
  concepts and 42 assets — with `kb sync pull`. Scope is set by [`sync.yaml`](/sync.yaml) and pinned by
  [`sync.lock.yaml`](/sync.lock.yaml).
* **Creation**: The import deliberately reaches past `docs/`. The bundles that came before it were seeded from
  `docs/spec/draft/` and `docs/design/draft/ir/` alone — two prose drafts — and so never saw the artifacts the format
  is actually defined by: `website/static/schemas/morphir-ir-*.yaml`, the v4 BDD fixtures and compliance feature, the
  published examples, `wit/morphir-ir/`, and the whole published spec tree at `docs/spec/ir/`. Several concepts in
  `morphir-ir-v4-draft` cite `schemas/v4/tree/module.yaml` and its siblings as the tiebreak for open questions; no
  such path exists in `finos/morphir`, at that commit or on `main`. The real schemas are the two files under
  `website/static/schemas/`, now mirrored here.
* **Creation**: Generated `morphir-ir-*.json` are excluded. They are produced from the YAML by
  `website/scripts/yaml-to-json-schemas.js` during upstream's Netlify build, so mirroring them would mean keeping two
  copies of one fact — and the two converters upstream uses do not produce byte-identical output.
