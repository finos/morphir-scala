package mill.local.plugins.ci.release

import mill.*
import mill.api.Result
import mill.api.ExternalModule
import scala.annotation.nowarn
import java.util.Base64
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

// In here for the Discover import
@nowarn("msg=Unused import")
object ReleaseSetupModule extends ExternalModule {

  def setup(): Task.Command[Unit] = Task.Command {
    setupGpg()()
  }

  /**
   * Ensures that your key is imported prio to signing and publishing.
   */
  def setupGpg(): Task[Unit] = Task.Anon {
    Task.log.info("Attempting to setup gpg")
    val envData = setupEnv()
    val pgpSecret = envData.pgpSecret.replaceAll("\\s", "")
    try {
      val decoded = new String(
        Base64.getDecoder.decode(pgpSecret.getBytes(StandardCharsets.UTF_8))
      )

      // https://dev.gnupg.org/T2313
      val imported = os
        .proc("gpg", "--batch", "--import", "--no-tty")
        .call(stdin = decoded)

      if (imported.exitCode != 0)
        Result.Failure(
          "Unable to import your pgp key. Make sure your secret is correct."
        )
    } catch {
      case e: IllegalArgumentException =>
        Result.Failure(
          s"Invalid secret, unable to decode it: ${e.getMessage()}"
        )
      case NonFatal(e) => Result.Failure(e.getMessage())
    }
  }

  /**
   * Ensures that the user has all the ENV variable set up that are necessary to both take care of pgp related stuff and
   * also publish to sonatype.
   * @return
   *   an Env case class
   */
  private def setupEnv(): Env = {
    val env              = sys.env
    val pgpSecret        = env.get("PGP_SECRET")
    val pgpPassword      = env.get("PGP_PASSPHRASE")
    val isTag            = env.get("GITHUB_REF").exists(_.startsWith("refs/tags"))
    val sonatypeUser     = env.get("SONATYPE_USERNAME")
    val sonatypePassword = env.get("SONATYPE_PASSWORD")

    if (pgpSecret.isEmpty) {
      throw new Exception("Missing PGP_SECRET. Make sure you have it set.")
    } else if (pgpPassword.isEmpty) {
      throw new Exception("Missing PGP_PASSPHRASE. Make sure you have it set.")
    } else if (sonatypeUser.isEmpty) {
      throw new Exception("Missing SONATYPE_USERNAME. Make sure you have it set.")
    } else if (sonatypePassword.isEmpty) {
      throw new Exception("Missing SONATYPE_PASSWORD. Make sure you have it set.")
    } else {
      Env(
        pgpSecret.get,
        pgpPassword.get,
        isTag,
        sonatypeUser.get,
        sonatypePassword.get
      )
    }
  }

  lazy val millDiscover = mill.api.Discover[this.type]
}
