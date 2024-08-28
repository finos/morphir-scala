package mill.local.plugins.ci.release

import mill._
import mill.api.Result
import mill.define.Command
import mill.define.ExternalModule
import mill.define.Task
import mill.eval.Evaluator
import scala.annotation.nowarn
import java.util.Base64
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

// In here for the Discover import
@nowarn("msg=Unused import")
object ReleaseSetupModule extends ExternalModule {

  def setup(ev: Evaluator): Command[Unit] = T.command {
    setupGpg()()
  }

  private val envTask: Task[Env] = setupEnv()

  /**
   * Ensures that your key is imported prio to signing and publishing.
   */
  def setupGpg(): Task[Unit] = T.task {
    T.log.info("Attempting to setup gpg")
    val pgpSecret = envTask().pgpSecret.replaceAll("\\s", "")
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
   *   a Env Task
   */
  private def setupEnv(): Task[Env] = T.input {
    val env              = T.ctx().env
    val pgpSecret        = env.get("PGP_SECRET")
    val pgpPassword      = env.get("PGP_PASSPHRASE")
    val isTag            = env.get("GITHUB_REF").exists(_.startsWith("refs/tags"))
    val sonatypeUser     = env.get("SONATYPE_USERNAME")
    val sonatypePassword = env.get("SONATYPE_PASSWORD")

    if (pgpSecret.isEmpty) {
      Result.Failure("Missing PGP_SECRET. Make sure you have it set.")
    } else if (pgpPassword.isEmpty) {
      Result.Failure("Missing PGP_PASSPHRASE. Make sure you have it set.")
    } else if (sonatypeUser.isEmpty) {
      Result.Failure("Missing SONATYPE_USERNAME. Make sure you have it set.")
    } else if (sonatypePassword.isEmpty) {
      Result.Failure("Missing SONATYPE_PASSWORD. Make sure you have it set.")
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

  import Discover._
  lazy val millDiscover: mill.define.Discover[this.type] =
    mill.define.Discover[this.type]
}