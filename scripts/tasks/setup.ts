#!/usr/bin/env bun
/**
 * Setup task - installs dependencies
 */
import { $ } from "bun";

console.log("📦 Setting up project...");

try {
  console.log("  Installing npm dependencies...");
  await $`bun install`;
  
  console.log("✅ Setup completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Setup failed:", error);
  process.exit(1);
}
