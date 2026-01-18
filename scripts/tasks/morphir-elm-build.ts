#!/usr/bin/env bun
/**
 * Build Morphir Elm evaluator tests
 */
import { $ } from "bun";

console.log("🔨 Building Morphir Elm evaluator tests...");

try {
  const dir = "examples/morphir-elm-projects/evaluator-tests";
  console.log(`  Building ${dir}...`);
  await $`cd ${dir} && morphir-elm make`;
  
  console.log("✅ Morphir Elm build completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Morphir Elm build failed:", error);
  process.exit(1);
}
