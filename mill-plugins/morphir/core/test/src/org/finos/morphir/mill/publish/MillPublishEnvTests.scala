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

    test("validate accepts a generated ephemeral key") {
      if !EphemeralPgp.gpgAvailable then {
        println("skipping ephemeral PgpSecret.validate: gpg not on PATH")
      } else {
        val passphrase = "morphir-ci-test-pass"
        val armored    = EphemeralPgp.generateArmoredSecret(passphrase)
        val millEnv    = MillSonatypeEnv.fromEnvOrThrow(
          Map(
            "GPG_PRIVATE_KEY"   -> armored,
            "GPG_PASSPHRASE"    -> passphrase,
            "SONATYPE_USERNAME" -> "dry-user",
            "SONATYPE_PASSWORD" -> "dry-pass"
          )
        )
        PgpSecret.validate(millEnv.pgpSecretBase64)
        assert(millEnv.pgpSecretBase64 == PgpSecret.toMillBase64(armored))
      }
    }
  }
}
