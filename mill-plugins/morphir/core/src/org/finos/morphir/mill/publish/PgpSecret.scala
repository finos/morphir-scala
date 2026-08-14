package org.finos.morphir.mill.publish

import java.nio.charset.StandardCharsets
import java.util.Base64

import scala.util.control.NonFatal

/**
 * Normalize CI-supplied PGP material to Mill's `MILL_PGP_SECRET_BASE64` form and optionally validate it with a
 * temporary `gpg --import`.
 *
 * Accepts armored plaintext (`BEGIN PGP PRIVATE KEY`) or base64 of that armor — the same dual input Morphir CI
 * historically accepted in its publish scripts.
 */
object PgpSecret {

  /**
   * Convert raw key material into the base64 string Mill expects for `MILL_PGP_SECRET_BASE64`.
   *
   * @param log
   *   optional progress messages (armor detected, base64 assumed, etc.)
   */
  def toMillBase64(rawKey: String, log: String => Unit = _ => ()): String = {
    val normalized = rawKey.replace("\r", "")
    if normalized.contains("BEGIN PGP PRIVATE KEY") then {
      log("Detected plaintext armored PGP key; encoding to base64 for Mill")
      Base64.getEncoder.encodeToString(normalized.getBytes(StandardCharsets.UTF_8))
    } else {
      val cleaned = normalized.filterNot(_.isWhitespace)
      try {
        val decoded = new String(Base64.getDecoder.decode(cleaned), StandardCharsets.UTF_8)
        if decoded.contains("PGP") || decoded.contains("PRIVATE") || decoded.contains("BEGIN") then {
          log("Detected base64-encoded PGP key")
          cleaned
        } else {
          log("Assuming base64 PGP key (decoded content lacked obvious PGP markers)")
          cleaned
        }
      } catch {
        case _: IllegalArgumentException =>
          log("Key was not valid base64; encoding as UTF-8 bytes")
          Base64.getEncoder.encodeToString(normalized.getBytes(StandardCharsets.UTF_8))
      }
    }
  }

  /**
   * Import the Mill-form base64 secret into a throwaway `GNUPGHOME` and fail if GPG rejects it or reports the key as
   * expired.
   */
  def validate(secretBase64: String, log: String => Unit = _ => ()): Unit = {
    log("Validating GPG key via temporary import")
    val decoded =
      try new String(Base64.getDecoder.decode(secretBase64), StandardCharsets.UTF_8)
      catch {
        case e: IllegalArgumentException =>
          throw PgpError.InvalidBase64(e.getMessage)
      }

    // macOS unix-socket paths are capped; keep GNUPGHOME short so S.gpg-agent.extra fits.
    val tempHome = shortGpgHome("mgv-")
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
          stdin = decoded,
          stdout = os.Pipe,
          stderr = os.Pipe,
          check = false
        )
      if imported.exitCode != 0 then
        throw PgpError.ImportFailed(imported.exitCode, imported.err.text())

      val listed = os
        .proc(
          "gpg",
          "--homedir",
          tempHome.toString,
          "--batch",
          "--pinentry-mode",
          "loopback",
          "--list-secret-keys"
        )
        .call(env = env, stdout = os.Pipe, stderr = os.Pipe, check = false)
      if listed.out.text().toLowerCase.contains("expired") then throw PgpError.KeyExpired

      log("GPG key imported successfully for validation")
    } catch {
      case e: PgpError => throw e
      case NonFatal(e) => throw PgpError.ValidationFailed(e.getMessage)
    } finally {
      os.proc("gpgconf", "--homedir", tempHome.toString, "--kill", "gpg-agent")
        .call(env = env, check = false, stdout = os.Pipe, stderr = os.Pipe)
      os.remove.all(tempHome)
    }
  }

  /** Prefer `/tmp` when it exists so GPG agent sockets stay under the macOS path cap. */
  private[publish] def shortGpgHome(prefix: String): os.Path = {
    val tmp = os.root / "tmp"
    if os.isDir(tmp) then os.temp.dir(dir = tmp, prefix = prefix)
    else os.temp.dir(prefix = prefix)
  }
}
