#!/usr/bin/env bun
/**
 * Format task - formats Scala and Elm code
 */
import { $ } from "bun";

console.log("🎨 Formatting code...");

try {
  // Format Scala code
  console.log("  Formatting Scala files...");
  await $`./mill Alias/run fmt`;
  
  console.log("✅ Format completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Format failed:", error);
  process.exit(1);
}
