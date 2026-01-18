#!/usr/bin/env bun
/**
 * Lint task - checks code formatting and style
 */
import { $ } from "bun";

console.log("🔍 Linting code...");

try {
  // Lint Scala code
  console.log("  Linting Scala files...");
  await $`./mill Alias/run checkfmt`;
  
  console.log("✅ Lint completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Lint failed:", error);
  process.exit(1);
}
