#!/usr/bin/env bun
/**
 * Test JVM task - runs JVM tests for all Scala versions
 */
import { $ } from "bun";

console.log("🧪 Running JVM tests...");

// Default Scala version
const defaultScalaVersion = "3.7.4";

// Get Scala versions from environment or use default
const scalaVersionsEnv = process.env.SCALA_VERSIONS;
const scalaVersions = scalaVersionsEnv 
  ? scalaVersionsEnv.split(",").map(v => v.trim())
  : [defaultScalaVersion];

console.log(`  Scala versions: ${scalaVersions.join(", ")}`);

try {
  // First build Morphir Elm
  console.log("  Building Morphir Elm...");
  await $`bun run build:morphir-elm`;
  
  // Run tests for each Scala version
  for (const scalaVersion of scalaVersions) {
    console.log(`  Running JVM tests for Scala ${scalaVersion}...`);
    await $`./mill -i -k -j 0 morphir[${scalaVersion}].__.jvm.__.compile + morphir[${scalaVersion}].__.jvm.publishArtifacts + morphir[${scalaVersion}].__.jvm.__.test`;
  }
  
  console.log("✅ JVM tests completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ JVM tests failed:", error);
  process.exit(1);
}
