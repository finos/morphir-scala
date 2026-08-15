package morphir.connector.github

import scala.scalajs.js

/**
 * Snapshot directory for kyo-test.
 *
 * Scala.js `sys.env` is an empty map. Mill sets Node `process.env` through jsEnvConfig.
 */
private[github] object SnapshotDir:
  def value: String =
    val raw = js.Dynamic.global.process.env.selectDynamic("MORPHIR_CONNECTOR_GITHUB_SNAPSHOT_DIR")
    if js.isUndefined(raw) || raw == null then "morphir/connector/github/test-snapshots"
    else raw.toString
