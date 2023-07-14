import mill._, scalalib._

object millbuild extends MillBuildRootModule {
  override def scalacOptions = super.scalacOptions() ++ Seq("-feature", "-deprecation", "-unchecked")
  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivy"com.geirsson::metaconfig-core:0.11.1",
    ivy"com.geirsson::metaconfig-sconfig:0.11.1"
  )
}
