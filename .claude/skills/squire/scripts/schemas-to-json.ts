#!/usr/bin/env bun
/**
 * squire schemas-to-json — Generate the Morphir IR JSON schemas from their YAML sources.
 *
 * Usage:
 *   bun schemas-to-json.ts [--from DIR] [--to DIR] [--check] [--all] [--json]
 *
 * Upstream keeps the schemas as YAML and generates the `.json` siblings from them with
 * `website/scripts/yaml-to-json-schemas.js`, which runs during the Netlify build and nowhere else. Two things follow
 * from that, and this script exists for both:
 *
 *   - Nothing verifies the committed `.json` still matches the `.yaml`. GitHub Actions does not run the generator,
 *     so a YAML edit merged without it leaves the served schema stale until the next deploy quietly rewrites it.
 *     `--check` is that missing verification.
 *   - The generator needs `js-yaml`, a devDependency, so it cannot run in a checkout nobody has run `npm install`
 *     in — which is every sparse reference checkout. Bun parses YAML natively, so this has no dependencies at all
 *     beyond the bun already pinned in .config/mise/config.toml.
 *
 * It reproduces the upstream generator exactly rather than reimplementing it: same file filter, same `$id` rewrite,
 * same `JSON.stringify(schema, null, 2) + "\n"`. That is verified, not assumed — `--check` against an untouched
 * reference checkout reports every file identical, and it is the reason this is safe to use when preparing an export.
 *
 * The knowledge base mirrors the YAML only. The JSON is derived, so it is build output: it lands under .dev/ unless
 * `--to` says otherwise, and only an export writes it into a checkout.
 */

import { resolve, basename } from "node:path";

// Upstream's generator matches `morphir-ir-*.yaml` and silently ignores morphir-config-v1 and morphir-project-v1,
// whose committed .json come from a separate Python converter. --all covers those too; the default does not, so that
// what this writes is what upstream's build would write.
const IR_ONLY = "morphir-ir-*.yaml";
const ALL = "morphir-*.yaml";

const DEFAULT_FROM = "kb/bundles/morphir/morphir-upstream/sources/website/static/schemas";
const DEFAULT_TO = ".dev/out/squire/schemas";

type Outcome = { file: string; status: "written" | "identical" | "drifted" | "missing"; detail?: string };

function flag(name: string): boolean {
  return process.argv.includes(`--${name}`);
}

function value(name: string, fallback: string): string {
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 && i + 1 < process.argv.length ? process.argv[i + 1] : fallback;
}

/** The conversion itself. Kept in one place so `--check` cannot drift from what a write would produce. */
function convert(yaml: string): string {
  const schema = Bun.YAML.parse(yaml) as Record<string, unknown>;
  const id = schema?.$id;
  if (typeof id === "string" && id.endsWith(".yaml")) {
    schema.$id = id.replace(/\.yaml$/, ".json");
  }
  return JSON.stringify(schema, null, 2) + "\n";
}

async function main(): Promise<number> {
  const check = flag("check");
  const asJson = flag("json");
  const from = resolve(value("from", DEFAULT_FROM));
  // In --check the sources and the JSON being judged sit side by side, which is how upstream stores them.
  const to = resolve(value("to", check ? from : DEFAULT_TO));

  const pattern = flag("all") ? ALL : IR_ONLY;
  const names = [...new Bun.Glob(pattern).scanSync(from)].sort();

  if (names.length === 0) {
    console.error(`ERROR: no files matching ${pattern} under ${from}`);
    return 1;
  }

  const outcomes: Outcome[] = [];
  for (const name of names) {
    const target = `${to}/${basename(name).replace(/\.yaml$/, ".json")}`;
    const generated = convert(await Bun.file(`${from}/${name}`).text());

    if (!check) {
      await Bun.write(target, generated);
      outcomes.push({ file: basename(target), status: "written" });
      continue;
    }

    const existing = Bun.file(target);
    if (!(await existing.exists())) {
      outcomes.push({ file: basename(target), status: "missing", detail: "no generated JSON beside the YAML" });
    } else if ((await existing.text()) === generated) {
      outcomes.push({ file: basename(target), status: "identical" });
    } else {
      outcomes.push({ file: basename(target), status: "drifted", detail: "regenerate it — the YAML has moved on" });
    }
  }

  const bad = outcomes.filter((o) => o.status === "drifted" || o.status === "missing");

  if (asJson) {
    console.log(JSON.stringify({ command: "schemas-to-json", from, to, check, ok: bad.length === 0, outcomes }, null, 2));
  } else {
    for (const o of outcomes) {
      const mark = o.status === "identical" || o.status === "written" ? "  " : "❌";
      console.log(`${mark} ${o.file.padEnd(42)} ${o.status}${o.detail ? ` — ${o.detail}` : ""}`);
    }
    console.log("");
    if (check && outcomes.every((o) => o.status === "missing")) {
      // The knowledge base mirrors the YAML only, so checking it against itself finds no JSON at all. Say that
      // rather than listing every file as a defect.
      console.log(`no generated JSON under ${to}`);
      console.log("--check compares a directory that holds both, such as a reference checkout;");
      console.log("to generate from the mirror instead, drop --check or run `mise run schemas:build`.");
    } else if (check) {
      console.log(bad.length === 0 ? `${outcomes.length} schema(s) in step with their YAML` : `${bad.length} of ${outcomes.length} schema(s) out of step`);
    } else {
      console.log(`wrote ${outcomes.length} schema(s) to ${to}`);
    }
  }

  return bad.length === 0 ? 0 : 1;
}

process.exit(await main());
