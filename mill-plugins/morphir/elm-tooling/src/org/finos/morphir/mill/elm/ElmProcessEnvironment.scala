package org.finos.morphir.mill.elm

object ElmProcessEnvironment {
  private val RetainedVariables = Set(
    "HTTP_PROXY",
    "HTTPS_PROXY",
    "NO_PROXY",
    "ALL_PROXY",
    "http_proxy",
    "https_proxy",
    "no_proxy",
    "all_proxy",
    "SSL_CERT_FILE",
    "SSL_CERT_DIR",
    "NODE_EXTRA_CA_CERTS",
    "SYSTEMROOT",
    "SystemRoot",
    "WINDIR",
    "COMSPEC",
    "PATHEXT"
  )

  def create(taskRoot: os.Path, ambient: Map[String, String]): Map[String, String] = {
    val home    = taskRoot / "home"
    val elmHome = taskRoot / "elm-home"
    val cache   = taskRoot / "cache"
    val temp    = taskRoot / "tmp"
    ambient.view.filterKeys(RetainedVariables).toMap ++ Map(
      "HOME"             -> home.toString,
      "USERPROFILE"      -> home.toString,
      "ELM_HOME"         -> elmHome.toString,
      "XDG_CACHE_HOME"   -> (cache / "xdg").toString,
      "npm_config_cache" -> (cache / "npm").toString,
      "TMPDIR"           -> temp.toString,
      "TMP"              -> temp.toString,
      "TEMP"             -> temp.toString
    )
  }

  def initialize(environment: Map[String, String]): Unit =
    Seq("HOME", "ELM_HOME", "XDG_CACHE_HOME", "npm_config_cache", "TMPDIR")
      .foreach(name => os.makeDir.all(os.Path(environment(name), os.pwd)))
}
