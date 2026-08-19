package org.finos.morphir.mill.publish.desktop

import utest.*

/**
 * End-to-end: stage raw per-platform output, canonicalize it with [[DesktopRelease]], fabricate a signature,
 * read the result back with [[ReleaseSnapshot.read]], and verify it with [[DesktopReleaseChecks]] — the same
 * pipeline `ci.desktop.verify` runs. This is the check that proves the job-boundary gap (digests computed on
 * one runner, assets crossing to another) is actually closed.
 */
object DesktopReleaseVerificationTests extends TestSuite {
  private val version   = "0.4.2"
  private val platforms = Seq(DesktopPlatform.MacAarch64, DesktopPlatform.WinAmd64)

  // Both MacAarch64 and WinAmd64 archive as zip (DesktopPlatform.scala); archiveMagicNumbers checks
  // the real zip signature, so the staged raw archives must carry it too, not plain text.
  private val zipMagic = Array[Byte](0x50, 0x4b, 0x03, 0x04)

  private def stageRawOutput(root: os.Path): Unit = {
    os.write.over(root / "mac-aarch64" / "raw-mac.zip", zipMagic ++ "mac-archive".getBytes, createFolders = true)
    os.write.over(root / "mac-aarch64" / "raw-mac.dmg", "mac-installer", createFolders = true)
    os.write.over(root / "win-amd64" / "raw-win.zip", zipMagic ++ "win-archive".getBytes, createFolders = true)
    os.write.over(root / "win-amd64" / "raw-win.exe", "win-installer", createFolders = true)
  }

  private def fabricateSignature(release: os.Path): Unit =
    os.write.over(
      release / s"${DesktopReleaseLayout.ChecksumsFileName}.asc",
      "-----BEGIN PGP SIGNATURE-----\n\nfabricated-for-testing\n-----END PGP SIGNATURE-----\n"
    )

  val tests = Tests {
    test("a freshly canonicalized, signed release passes verification") {
      val staging = os.temp.dir()
      val release = os.temp.dir()
      stageRawOutput(staging)
      DesktopRelease.canonicalize(staging, release, version, platforms).toOption.get
      fabricateSignature(release)

      val snapshot = ReleaseSnapshot.read(release, version, platforms)
      val outcomes = ReleaseVerifier.run(DesktopReleaseChecks.defaults(requireSignature = true), snapshot)
      val problems = outcomes.flatMap(o => o.problems)
      assert(problems.isEmpty)
    }

    test("an asset corrupted after canonicalization fails verification and names the asset") {
      val staging = os.temp.dir()
      val release = os.temp.dir()
      stageRawOutput(staging)
      DesktopRelease.canonicalize(staging, release, version, platforms).toOption.get
      fabricateSignature(release)

      // Simulate corruption crossing the job boundary (artifact upload/download): overwrite the
      // asset's bytes without touching its sidecar.
      val corrupted = "morphir-desktop-mac-aarch64-0.4.2.zip"
      os.write.over(release / corrupted, "not-the-original-bytes-at-all")

      val snapshot = ReleaseSnapshot.read(release, version, platforms)
      val outcomes = ReleaseVerifier.run(DesktopReleaseChecks.defaults(requireSignature = true), snapshot)
      val problems = outcomes.flatMap(o => o.problems)
      assert(problems.nonEmpty)
      assert(problems.exists(_.contains(corrupted)))
    }

    test("verification without a signature required passes a canonicalized release that has no .asc yet") {
      val staging = os.temp.dir()
      val release = os.temp.dir()
      stageRawOutput(staging)
      DesktopRelease.canonicalize(staging, release, version, platforms).toOption.get

      val snapshot = ReleaseSnapshot.read(release, version, platforms)
      val outcomes = ReleaseVerifier.run(DesktopReleaseChecks.defaults(requireSignature = false), snapshot)
      val problems = outcomes.flatMap(o => o.problems)
      assert(problems.isEmpty)
    }
  }
}
