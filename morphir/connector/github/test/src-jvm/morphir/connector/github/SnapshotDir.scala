package morphir.connector.github

/** Snapshot directory for kyo-test. Mill injects an absolute path via forkEnv. */
private[github] object SnapshotDir:
  def value: String =
    sys.env.getOrElse(
      "MORPHIR_CONNECTOR_GITHUB_SNAPSHOT_DIR",
      "morphir/connector/github/test-snapshots"
    )
