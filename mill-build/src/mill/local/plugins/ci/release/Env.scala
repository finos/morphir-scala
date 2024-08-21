package mill.local.plugins.ci.release

/**
 * The env variables that are necessary to sign and publish
 *
 * @param pgpSecret
 *   base64 encoded secret
 * @param pgpPassword
 *   password to unlock your secret
 * @param isTag
 *   whether or not this is a stable release or not
 * @param sonatypeUser
 *   your sonatype user
 * @param sonatypePassword
 *   your sontatype password
 */
private[release] final case class Env(
    pgpSecret: String,
    pgpPassword: String,
    isTag: Boolean,
    sonatypeUser: String,
    sonatypePassword: String
) {

  /**
   * Sonatype creds in the format that Mill uses
   */
  val sonatypeCreds: String = s"${sonatypeUser}:${sonatypePassword}"
}

object Env {
  implicit def rw: upickle.default.ReadWriter[Env] =
    upickle.default.macroRW
}