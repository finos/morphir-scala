#!/usr/bin/env bun
/**
 * ci:local task - runs the same core steps as CI (lint, build, tests)
 */
import { $ } from "bun";

const scalaVersions = process.env.SCALA_VERSIONS ?? "3.7.4";
const scalaVersionForJs = scalaVersions.split(",")[0].trim();

console.log("🧪 Running CI-local workflow...");
console.log(`  Using SCALA_VERSIONS=${scalaVersions}`);

try {
  console.log("  Installing toolchain dependencies...");
  await $`mise run setup`;

  console.log("  Linting code...");
  await $`mise run lint`;

  console.log("  Building Morphir IR (elm)...");
  await $`mise run build:morphir-elm`;

  console.log("  Running JVM tests...");
  await $`SCALA_VERSIONS=${scalaVersions} mise run test:jvm`;

  console.log("  Running JS compile/test suite...");
  await $`./mill -i -k -j 0 "morphir[${scalaVersionForJs}].__.js.__.compile" + "morphir[${scalaVersionForJs}].__.js.publishArtifacts" + "morphir[${scalaVersionForJs}].__.js.__.test"`;

  console.log("✅ CI-local workflow completed successfully");
  process.exit(0);
} catch (error) {
  console.error("❌ CI-local workflow failed:", error);
  process.exit(1);
}
