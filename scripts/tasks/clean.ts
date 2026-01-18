#!/usr/bin/env bun
/**
 * Clean task - removes build artifacts
 */
import { $ } from "bun";

console.log("🧹 Cleaning build artifacts...");

try {
  await $`rm -rf out/`;
  console.log("✅ Clean completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Clean failed:", error);
  process.exit(1);
}
