package org.finos.morphir.mill.publish

/**
 * Failures from PGP key normalization, GPG validation, or Mill Sonatype env assembly. Safe to throw or pattern-match;
 * each case carries a user-facing message.
 */
enum PgpError(message: String) extends Exception(message) {
  case MissingEnv(names: Seq[String])
      extends PgpError(
        s"Missing required environment variables for Sonatype upload: ${names.mkString(", ")}"
      )
  case InvalidBase64(detail: String)
      extends PgpError(s"Failed to base64-decode GPG key: $detail")
  case ImportFailed(exitCode: Int, stderr: String)
      extends PgpError(s"Failed to import GPG key (exit $exitCode): $stderr")
  case KeyExpired
      extends PgpError("GPG key appears to be expired")
  case ValidationFailed(detail: String)
      extends PgpError(s"GPG key validation failed: $detail")
}
