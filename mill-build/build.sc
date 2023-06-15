import mill._, scalalib._

object millbuild extends MillBuildRootModule {
  override def scalacOptions = super.scalacOptions() ++ Seq("-feature", "-deprecation", "-unchecked")
}
