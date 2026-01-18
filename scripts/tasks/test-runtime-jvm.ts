#!/usr/bin/env bun
/**
 * Test Runtime JVM task - runs runtime JVM tests for all Scala versions
 */
import { $ } from "bun";

console.log("🧪 Running Runtime JVM tests...");

// Default Scala version
const defaultScalaVersion = "3.7.4";

// Get Scala versions from environment or use default
const scalaVersionsEnv = process.env.SCALA_VERSIONS;
const scalaVersions = scalaVersionsEnv 
  ? scalaVersionsEnv.split(",").map(v => v.trim())
  : [defaultScalaVersion];

console.log(`  Scala versions: ${scalaVersions.join(", ")}`);

try {
  // First build all Elm projects
  console.log("  Building Elm projects...");
  await $`bun run build:elm`;
  
  // Run tests for each Scala version
  for (const scalaVersion of scalaVersions) {
    console.log(`  Running Runtime JVM tests for Scala ${scalaVersion}...`);
    await $`./mill -i -k -j 0 morphir[${scalaVersion}].__.runtime.jvm.__.compile + morphir[${scalaVersion}].__.runtime.jvm.publishArtifacts + morphir[${scalaVersion}].__.runtime.jvm.__.test`;
  }
  
  console.log("✅ Runtime JVM tests completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ Runtime JVM tests failed:", error);
  process.exit(1);
}
