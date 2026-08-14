package org.finos.morphir.mill.publish

/** Throwaway GPG material for mill-morphir-core tests. Not used in publication. */
object EphemeralPgp {

  def gpgAvailable: Boolean =
    try
      os.proc("gpg", "--version").call(check = false, stdout = os.Pipe, stderr = os.Pipe).exitCode == 0
    catch {
      case _: Throwable => false
    }

  /**
   * Generate an armored secret key in a throwaway `GNUPGHOME`. Prefers Ed25519; falls back to RSA 2048 if the local
   * GnuPG cannot emit that curve.
   */
  def generateArmoredSecret(passphrase: String): String = {
    val home = PgpSecret.shortGpgHome("mg-")
    val env  = sys.env.toMap
      .updated("GNUPGHOME", home.toString)
      - "GPG_AGENT_INFO"
      - "GPG_TTY"
    try {
      os.write.over(home / "gpg-agent.conf", "allow-loopback-pinentry\ndisable-scdaemon\n")
      os.write.over(home / "gpg.conf", "pinentry-mode loopback\n")
      launchAgent(env, home)
      val eddsa = generate(env, home, eddsaBatch(passphrase))
      if eddsa.exitCode != 0 then {
        val rsa = generate(env, home, rsaBatch(passphrase))
        if rsa.exitCode != 0 then
          throw new RuntimeException(
            s"gpg --generate-key failed (eddsa: ${eddsa.err.text()}; rsa: ${rsa.err.text()})"
          )
      }
      val exported = os
        .proc(
          "gpg",
          "--homedir",
          home.toString,
          "--batch",
          "--yes",
          "--pinentry-mode",
          "loopback",
          "--passphrase",
          passphrase,
          "--export-secret-keys",
          "--armor"
        )
        .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe)
      val armor = exported.out.text()
      if exported.exitCode != 0 || !armor.contains("BEGIN PGP PRIVATE KEY") then
        throw new RuntimeException(s"gpg --export-secret-keys failed: ${exported.err.text()}\n$armor")
      armor
    } finally {
      os.proc("gpgconf", "--homedir", home.toString, "--kill", "gpg-agent")
        .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe)
      os.remove.all(home)
    }
  }

  private def launchAgent(env: Map[String, String], home: os.Path): Unit = {
    val daemon = os
      .proc(
        "gpg-agent",
        "--homedir",
        home.toString,
        "--daemon",
        "--allow-loopback-pinentry"
      )
      .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe)
    val ping = os
      .proc("gpg-connect-agent", "--homedir", home.toString, "/bye")
      .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe)
    if ping.exitCode != 0 then
      throw new RuntimeException(
        s"gpg-agent is not reachable (daemon exit ${daemon.exitCode}: ${daemon.err.text()}; ping: ${ping.err.text()})"
      )
  }

  private def generate(env: Map[String, String], home: os.Path, body: String): os.CommandResult = {
    val batch = home / "batch"
    os.write.over(batch, body)
    os.proc(
      "gpg",
      "--homedir",
      home.toString,
      "--batch",
      "--pinentry-mode",
      "loopback",
      "--generate-key",
      batch.toString
    )
      .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe)
  }

  private def eddsaBatch(passphrase: String): String =
    s"""%echo generating ephemeral morphir ci test key
Key-Type: EDDSA
Key-Curve: Ed25519
Key-Usage: sign
Name-Real: Morphir CI Test
Name-Email: morphir-ci-test@example.invalid
Expire-Date: 1d
Passphrase: $passphrase
%commit
"""

  private def rsaBatch(passphrase: String): String =
    s"""%echo generating ephemeral morphir ci test key
Key-Type: RSA
Key-Length: 2048
Key-Usage: sign
Name-Real: Morphir CI Test
Name-Email: morphir-ci-test@example.invalid
Expire-Date: 1d
Passphrase: $passphrase
%commit
"""
}
