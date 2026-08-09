@main def runMorphirElmLegacyBridgeSmoke(): Unit = {
  val workspace   = os.pwd
  val millVersion = os.read(workspace / ".mill-version").trim
  val target      = s"mill-plugins.morphir.elm[$millVersion].test"
  val selector =
    "org.finos.morphir.mill.elm.morphir.MorphirElmProjectTests." +
      "legacy metabuild module names retain the Elm adapter surface"
  os.proc(
    workspace / "mill",
    "--no-daemon",
    "--no-build-lock",
    "--ticker",
    "false",
    target,
    selector
  ).call(cwd = workspace, stdout = os.Inherit, stderr = os.Inherit)
}
