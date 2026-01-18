#!/usr/bin/env bun
/**
 * Setup IDEA task - generates IntelliJ IDEA project files
 */
import { $ } from "bun";

console.log("💡 Setting up IntelliJ IDEA project...");

try {
  await $`./mill mill.idea.GenIdea/idea`;
  
  console.log("✅ IDEA setup completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ IDEA setup failed:", error);
  process.exit(1);
}
