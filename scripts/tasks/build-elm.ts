#!/usr/bin/env bun
/**
 * Build Elm projects task
 */
import { $ } from "bun";
import { join } from "path";

console.log("🔨 Building Elm projects...");

const elmProjectDirs = [
  "examples/morphir-elm-projects/evaluator-tests",
  "examples/morphir-elm-projects/defaults-tests", 
  "examples/morphir-elm-projects/finance",
  "morphir-elm/sdks/morphir-unit-test",
];

try {
  for (const dir of elmProjectDirs) {
    const morphirJsonPath = join(dir, "morphir.json");
    try {
      await Bun.file(morphirJsonPath).text();
      console.log(`  Building ${dir}...`);
      await $`cd ${dir} && morphir-elm make`;
    } catch {
      console.log(`  Skipping ${dir} (no morphir.json)`);
    }
  }
  
  console.log("✅ Elm build completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Elm build failed:", error);
  process.exit(1);
}
