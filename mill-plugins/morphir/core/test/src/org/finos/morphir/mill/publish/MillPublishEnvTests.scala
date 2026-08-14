package org.finos.morphir.mill.publish

import java.nio.charset.StandardCharsets
import java.util.Base64

import utest.*

object MillPublishEnvTests extends TestSuite {

  private val armoredKey =
    """-----BEGIN PGP PRIVATE KEY BLOCK-----
      |Version: test
      |
      |xYzDummyMaterialForUnitTestsOnly=
      |-----END PGP PRIVATE KEY BLOCK-----
      |""".stripMargin

  private val armoredBase64 =
    Base64.getEncoder.encodeToString(armoredKey.getBytes(StandardCharsets.UTF_8))

  /** Unprotected RSA fixture Mill's own publish tests use. Avoids `gpg --generate-key`, which can hang on GHA. */
  private val millTestKeyBase64 =
    "LS0tLS1CRUdJTiBQR1AgUFJJVkFURSBLRVkgQkxPQ0stLS0tLQoKeFZnRWFHekhpeFlKS3dZQkJBSGFSdzhCQVFkQWxQamhsaGo5MUtZUnhDQXFtaUZNMjR1UEVDL0kxemR0CnlWS2dRR1lENHZZQUFQOW9jK0ZFQzQ2dkt6b0tNWVE3M1Jvemh4UDE3WWhUZnZwRFBwYk1CZHNZQ2c2RQp6VEpwYnk1bmFYUm9kV0l1WVhKMGRYSmhlaTUwWlhOMFVISnZhbVZqZENCaWIzUWdQR0Z6UUdGeWRIVnkKWVhvdWJtVjBQc0tNQkJBV0NnQWRCUUpvYk1lTEJBc0pCd2dERlFnS0JCWUFBZ0VDR1FFQ0d3TUNIZ0VBCklRa1FBMkRDK3lxemF1RVdJUVRnUmJWQ05LcVpxRTFkdDB3RFlNTDdLck5xNFR1L0FQNHRDYzZpYWNUdQpZVEJBa2Q3UDZOM1E1VTZjbGdnSElVQ2lRL3lIbmFvVHZ3RUExbU92M2MydEVORGtrdnF5Ujl2YVhWNHEKZlBEckNDRmRTUTR0anpMY3hnVEhYUVJvYk1lTEVnb3JCZ0VFQVpkVkFRVUJBUWRBUHpzMjV5RERLSC80Cm1KNmtMU1dLSExITXJEWUZMWGVHOTNWRTluSVY0Q0FEQVFnSEFBRC9aQ1hVMDhqMkZTU2VYQWdZaFZzNwp2akVDQjQweTA2TjdaM0pqaitCSko3Z08xc0o0QkJnV0NBQUpCUUpvYk1lTEFoc01BQ0VKRUFOZ3d2c3EKczJyaEZpRUU0RVcxUWpTcW1haE5YYmRNQTJEQyt5cXphdUgrY2dEL1QxRUVkVDl1WnR6L255bGk1OHR0CjYxaWNLcndyU3kzSTBBRDNYWWErcm40QS9qWEZlZXNsNVBZZWtpU0ZzNVZGNUczRVNpWmY0amJxZXlOWQpLd09ENVIwSwo9WDhSdQotLS0tLUVORCBQR1AgUFJJVkFURSBLRVkgQkxPQ0stLS0tLQo="

  val tests = Tests {
    test("toMillBase64 encodes armored plaintext") {
      val encoded = PgpSecret.toMillBase64(armoredKey)
      assert(encoded == armoredBase64)
      assert(!encoded.contains("BEGIN PGP"))
    }

    test("toMillBase64 passes through base64 of armor") {
      assert(PgpSecret.toMillBase64(armoredBase64) == armoredBase64)
      // whitespace in base64 inputs is stripped
      val spaced = armoredBase64.grouped(16).mkString("\n")
      assert(PgpSecret.toMillBase64(spaced) == armoredBase64)
    }

    test("fromEnv accepts Morphir CI GPG_* names") {
      val env = Map(
        "GPG_PRIVATE_KEY"   -> armoredKey,
        "GPG_PASSPHRASE"    -> "secret-phrase",
        "SONATYPE_USERNAME" -> "user",
        "SONATYPE_PASSWORD" -> "pass"
      )
      val result = MillSonatypeEnv.fromEnv(env)
      assert(result.isRight)
      val millEnv = result.toOption.get
      assert(millEnv.pgpSecretBase64 == armoredBase64)
      assert(millEnv.pgpPassphrase == "secret-phrase")
      assert(millEnv.sonatypeUsername == "user")
      assert(millEnv.sonatypePassword == "pass")
      assert(
        millEnv.toProcessEnv == Map(
          "MILL_PGP_SECRET_BASE64" -> armoredBase64,
          "MILL_PGP_PASSPHRASE"    -> "secret-phrase",
          "MILL_SONATYPE_USERNAME" -> "user",
          "MILL_SONATYPE_PASSWORD" -> "pass"
        )
      )
    }

    test("fromEnv falls back to legacy PGP_SECRET names") {
      val env = Map(
        "PGP_SECRET"        -> armoredBase64,
        "PGP_PASSPHRASE"    -> "legacy-phrase",
        "SONATYPE_USERNAME" -> "user",
        "SONATYPE_PASSWORD" -> "pass"
      )
      val millEnv = MillSonatypeEnv.fromEnvOrThrow(env)
      assert(millEnv.pgpSecretBase64 == armoredBase64)
      assert(millEnv.pgpPassphrase == "legacy-phrase")
    }

    test("fromEnv prefers GPG_PRIVATE_KEY over PGP_SECRET") {
      val env = Map(
        "GPG_PRIVATE_KEY"   -> armoredKey,
        "PGP_SECRET"        -> "should-not-win",
        "GPG_PASSPHRASE"    -> "from-gpg",
        "PGP_PASSPHRASE"    -> "from-pgp",
        "SONATYPE_USERNAME" -> "user",
        "SONATYPE_PASSWORD" -> "pass"
      )
      val millEnv = MillSonatypeEnv.fromEnvOrThrow(env)
      assert(millEnv.pgpSecretBase64 == armoredBase64)
      assert(millEnv.pgpPassphrase == "from-gpg")
    }

    test("fromEnv reports missing variables") {
      val result = MillSonatypeEnv.fromEnv(Map.empty)
      assert(result.isLeft)
      result.left.toOption.get match {
        case PgpError.MissingEnv(names) =>
          assert(names.exists(_.contains("GPG_PRIVATE_KEY")))
          assert(names.exists(_.contains("SONATYPE_USERNAME")))
        case _ =>
          assert(false)
      }
    }

    test("toMillBase64 encodes non-base64 plaintext as UTF-8") {
      val raw     = "!!!not-valid-base64!!!"
      val encoded = PgpSecret.toMillBase64(raw)
      assert(encoded == Base64.getEncoder.encodeToString(raw.getBytes(StandardCharsets.UTF_8)))
    }

    test("posixQuote wraps and escapes single quotes") {
      assert(MillSonatypeEnv.posixQuote("plain") == "'plain'")
      assert(MillSonatypeEnv.posixQuote("it's") == "'it'\\''s'")
    }

    test("requireWritableDest refuses workspace and out/ paths") {
      val workspace = os.root / "work" / "morphir-scala"
      val allowed   = os.root / "tmp" / "mill-publish.env"
      assert(MillPublishEnvFile.requireWritableDest(allowed, workspace) == allowed)
      try {
        MillPublishEnvFile.requireWritableDest(workspace / "out" / "publish.env", workspace)
        assert(false)
      } catch {
        case _: IllegalArgumentException => ()
      }
      try {
        MillPublishEnvFile.requireWritableDest(workspace / "secrets.env", workspace)
        assert(false)
      } catch {
        case _: IllegalArgumentException => ()
      }
    }

    test("toDotenv round-trips through bash source") {
      val millEnv = MillSonatypeEnv(
        pgpSecretBase64 = armoredBase64,
        pgpPassphrase = "it's a 'secret'",
        sonatypeUsername = "user",
        sonatypePassword = "p@ss word"
      )
      val file = os.temp(contents = millEnv.toDotenv, prefix = "mill-publish-env-")
      try {
        def read(name: String): String = {
          val script =
            s"""set -a
               |source ${MillSonatypeEnv.posixQuote(file.toString)}
               |set +a
               |printf '%s' "$$$name"
               |""".stripMargin
          val result = os.proc("bash", "-c", script).call(check = false, stdout = os.Pipe, stderr = os.Pipe)
          assert(result.exitCode == 0)
          result.out.text()
        }
        assert(read("MILL_PGP_SECRET_BASE64") == armoredBase64)
        assert(read("MILL_PGP_PASSPHRASE") == "it's a 'secret'")
        assert(read("MILL_SONATYPE_USERNAME") == "user")
        assert(read("MILL_SONATYPE_PASSWORD") == "p@ss word")
      } finally os.remove(file)
    }

    test("validate imports with gpg when available") {
      if !EphemeralPgp.gpgAvailable then {
        println("skipping PgpSecret.validate: gpg not on PATH")
      } else {
        try {
          PgpSecret.validate(armoredBase64)
          assert(false)
        } catch {
          case _: PgpError.ImportFailed | _: PgpError.ValidationFailed => ()
          case other: Throwable                                        =>
            throw other
        }
      }
    }

    test("validate accepts a converted mill test key") {
      if !EphemeralPgp.gpgAvailable then {
        println("skipping PgpSecret.validate: gpg not on PATH")
      } else {
        val armored = new String(Base64.getDecoder.decode(millTestKeyBase64), StandardCharsets.UTF_8)
        val millEnv = MillSonatypeEnv.fromEnvOrThrow(
          Map(
            "GPG_PRIVATE_KEY"   -> armored,
            "GPG_PASSPHRASE"    -> "unused",
            "SONATYPE_USERNAME" -> "dry-user",
            "SONATYPE_PASSWORD" -> "dry-pass"
          )
        )
        PgpSecret.validate(millEnv.pgpSecretBase64)
        assert(millEnv.pgpSecretBase64 == PgpSecret.toMillBase64(armored))
        assert(!millEnv.pgpSecretBase64.contains("BEGIN PGP"))
      }
    }

    test("validate accepts base64 of a binary secret-key export") {
      if !EphemeralPgp.gpgAvailable then {
        println("skipping PgpSecret.validate binary export: gpg not on PATH")
      } else {
        val armored  = new String(Base64.getDecoder.decode(millTestKeyBase64), StandardCharsets.UTF_8)
        val tempHome = PgpSecret.shortGpgHome("mbt-")
        val env      = sys.env.toMap.updated("GNUPGHOME", tempHome.toString) - "GPG_AGENT_INFO" - "GPG_TTY"
        try {
          val imported = os
            .proc(
              "gpg",
              "--homedir",
              tempHome.toString,
              "--batch",
              "--pinentry-mode",
              "loopback",
              "--import",
              "--no-tty"
            )
            .call(
              env = env,
              stdin = armored,
              stdout = os.Pipe,
              stderr = os.Pipe,
              check = false,
              timeout = 15_000L
            )
          assert(imported.exitCode == 0)

          val exported = os
            .proc(
              "gpg",
              "--homedir",
              tempHome.toString,
              "--batch",
              "--pinentry-mode",
              "loopback",
              "--export-secret-keys"
            )
            .call(env = env, stdout = os.Pipe, stderr = os.Pipe, check = false, timeout = 15_000L)
          assert(exported.exitCode == 0)
          val binaryBytes = exported.out.bytes
          assert(binaryBytes.nonEmpty)
          assert(!(new String(binaryBytes, StandardCharsets.UTF_8).contains("BEGIN PGP")))

          val binaryBase64 = Base64.getEncoder.encodeToString(binaryBytes)
          val millForm     = PgpSecret.toMillBase64(binaryBase64)
          assert(millForm == binaryBase64.filterNot(_.isWhitespace))
          PgpSecret.validate(millForm)
        } finally {
          os.proc("gpgconf", "--homedir", tempHome.toString, "--kill", "gpg-agent")
            .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe, timeout = 15_000L)
          os.remove.all(tempHome)
        }
      }
    }
  }
}
