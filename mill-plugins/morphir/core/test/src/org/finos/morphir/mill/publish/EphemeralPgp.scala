package org.finos.morphir.mill.publish

/** Test-only GPG probes. Not used in publication. */
object EphemeralPgp {

  private val GpgTimeoutMs = 15_000L

  def gpgAvailable: Boolean =
    try
      os.proc("gpg", "--version")
        .call(check = false, stdout = os.Pipe, stderr = os.Pipe, timeout = GpgTimeoutMs)
        .exitCode == 0
    catch {
      case _: Throwable => false
    }
}
