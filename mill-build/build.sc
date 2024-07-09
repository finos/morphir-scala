import mill._, scalalib._

object millbuild extends MillBuildRootModule {
  override def scalacOptions = super.scalacOptions() ++ Seq("-feature", "-deprecation", "-unchecked")
  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivy"dev.zio::zio:2.1.5",
    ivy"dev.zio::zio-config:4.0.2",
    ivy"dev.zio::zio-config-magnolia:4.0.2",
    ivy"dev.zio::zio-config-typesafe:4.0.2",
    ivy"dev.zio::zio-config-yaml:4.0.0-RC16",
    ivy"dev.zio::zio-config-refined:4.0.2"
  )
}
