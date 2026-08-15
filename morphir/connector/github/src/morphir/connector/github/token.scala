package morphir.connector.github

export morphir.connector.github.internal.Token

import kyo.*

/** GitHub access token from a process flag. Empty by default. Env `MORPHIR_CONNECTOR_GITHUB_TOKEN`. */
object token extends StaticFlag[String]("")

/**
 * Process env `GITHUB_TOKEN`. GitHub Actions workflows must explicitly export `github.token` or `secrets.GITHUB_TOKEN`
 * under this name.
 */
object GITHUB_TOKEN:
  val name: String             = "GITHUB_TOKEN"
  private val resolved: String = Flag(name, "")
  def apply(): String          = resolved
