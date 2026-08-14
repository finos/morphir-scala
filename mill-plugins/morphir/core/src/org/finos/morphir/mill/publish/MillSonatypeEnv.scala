package org.finos.morphir.mill.publish

/**
 * Credentials Mill's Sonatype Central publisher reads from the process environment.
 */
final case class MillSonatypeEnv(
    pgpSecretBase64: String,
    pgpPassphrase: String,
    sonatypeUsername: String,
    sonatypePassword: String
) {
  def toProcessEnv: Map[String, String] = Map(
    "MILL_PGP_SECRET_BASE64" -> pgpSecretBase64,
    "MILL_PGP_PASSPHRASE"    -> pgpPassphrase,
    "MILL_SONATYPE_USERNAME" -> sonatypeUsername,
    "MILL_SONATYPE_PASSWORD" -> sonatypePassword
  )

  /** POSIX dotenv body for `set -a; source file; set +a`. Values are single-quoted. */
  def toDotenv: String = MillSonatypeEnv.toDotenv(toProcessEnv)
}

object MillSonatypeEnv {

  private val CiKeyNames = EnvKeyNames(
    secret = Seq("GPG_PRIVATE_KEY", "PGP_SECRET"),
    passphrase = Seq("GPG_PASSPHRASE", "PGP_PASSPHRASE"),
    username = Seq("SONATYPE_USERNAME"),
    password = Seq("SONATYPE_PASSWORD")
  )

  private final case class EnvKeyNames(
      secret: Seq[String],
      passphrase: Seq[String],
      username: Seq[String],
      password: Seq[String]
  )

  /**
   * Build Mill env from a process/task environment map.
   *
   * Prefers Morphir CI names (`GPG_PRIVATE_KEY`, `GPG_PASSPHRASE`) and falls back to the legacy `PGP_SECRET` /
   * `PGP_PASSPHRASE` pair used by older local release helpers.
   *
   * Does not run GPG validation; callers that need a live import should call [[PgpSecret.validate]] on
   * `pgpSecretBase64` afterward.
   */
  def fromEnv(
      env: Map[String, String],
      log: String => Unit = _ => ()
  ): Either[PgpError, MillSonatypeEnv] = {
    def read(names: Seq[String]): Option[String] =
      names.view
        .flatMap(name => env.get(name).filterNot(_.isBlank))
        .headOption

    val missing = Seq(
      "GPG_PRIVATE_KEY|PGP_SECRET"    -> read(CiKeyNames.secret),
      "GPG_PASSPHRASE|PGP_PASSPHRASE" -> read(CiKeyNames.passphrase),
      "SONATYPE_USERNAME"             -> read(CiKeyNames.username),
      "SONATYPE_PASSWORD"             -> read(CiKeyNames.password)
    ).collect { case (label, None) => label }

    if missing.nonEmpty then Left(PgpError.MissingEnv(missing))
    else
      Right(
        MillSonatypeEnv(
          pgpSecretBase64 = PgpSecret.toMillBase64(read(CiKeyNames.secret).get, log),
          pgpPassphrase = read(CiKeyNames.passphrase).get,
          sonatypeUsername = read(CiKeyNames.username).get,
          sonatypePassword = read(CiKeyNames.password).get
        )
      )
  }

  /** Same as [[fromEnv]] but throws [[PgpError]] on failure. */
  def fromEnvOrThrow(
      env: Map[String, String],
      log: String => Unit = _ => ()
  ): MillSonatypeEnv =
    fromEnv(env, log).fold(throw _, identity)

  /** Quote a value so `source` in bash restores it byte-for-byte. */
  def posixQuote(value: String): String =
    "'" + value.replace("'", "'\\''") + "'"

  def toDotenv(env: Map[String, String]): String =
    env.toSeq.sortBy(_._1).map { (k, v) => s"$k=${posixQuote(v)}" }.mkString("", "\n", "\n")
}
