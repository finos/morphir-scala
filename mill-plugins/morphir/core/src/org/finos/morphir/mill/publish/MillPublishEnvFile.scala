package org.finos.morphir.mill.publish

/** Destination checks for `ci.sonatype.writeMillEnv --path`. */
object MillPublishEnvFile {

  /**
   * Mill's `os.Path` argument is always absolute; a relative `--path` is resolved against the workspace. Refuse
   * anything under the workspace (including `out/`) so secrets do not land in cache uploads or the git tree. GHA passes
   * `$RUNNER_TEMP`.
   */
  def requireWritableDest(path: os.Path, workspaceRoot: os.Path): os.Path = {
    val outRoot = workspaceRoot / "out"
    if path.startsWith(outRoot) then
      throw new IllegalArgumentException(s"refusing to write Mill publish env under $outRoot")
    if path.startsWith(workspaceRoot) then
      throw new IllegalArgumentException(
        s"writeMillEnv --path must be outside the workspace, got $path"
      )
    path
  }
}
