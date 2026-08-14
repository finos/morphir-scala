package mill.local.plugins.ci.release

import mill.*
import mill.api.ExternalModule
import org.finos.morphir.mill.publish.{MillSonatypeEnv, PgpSecret}

import scala.annotation.nowarn

/**
 * Local escape hatch to convert and validate Sonatype + GPG credentials before publish.
 *
 * Mill's publisher reads `MILL_PGP_*` from process env, not the user keyring. This command
 * validates the converted secret in a throwaway `GNUPGHOME`; it does not import into the
 * operator's real keyring. Env conversion lives in mill-morphir-core
 * (`org.finos.morphir.mill.publish`). Accepts Morphir CI names (`GPG_PRIVATE_KEY`) or the
 * legacy `PGP_SECRET` pair.
 */
@nowarn("msg=Unused import")
object ReleaseSetupModule extends ExternalModule {

  def setup(): Task.Command[Unit] = Task.Command {
    setupGpg()()
  }

  /**
   * Validates that the converted secret imports in a throwaway GNUPGHOME, and that Mill-facing
   * `MILL_*` env names can be assembled. Does not print credential values.
   */
  def setupGpg(): Task[Unit] = Task.Anon {
    Task.log.info("Validating Sonatype publish env via org.finos.morphir.mill.publish")
    val millEnv = MillSonatypeEnv.fromEnvOrThrow(sys.env, msg => Task.log.info(msg))
    PgpSecret.validate(millEnv.pgpSecretBase64, msg => Task.log.info(msg))
    Task.log.info(
      s"Sonatype credentials present; MILL_* env keys: ${millEnv.toProcessEnv.keys.toSeq.sorted.mkString(", ")}"
    )
  }

  lazy val millDiscover = mill.api.Discover[this.type]
}
