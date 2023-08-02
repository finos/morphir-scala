lazy val morphirSbtPlugin = project
  .in(file("."))
  .settings(
    name      := "sbt-morphir",
    sbtPlugin := true
  )
