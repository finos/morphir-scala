package millbuild

import mill._, scalalib._, scalafmt._
import java.util.Properties
trait CommonCrossScalaModule extends CrossScalaModule with CommonCoursierModule with ScalafmtModule { self =>

  def isScala3: T[Boolean] = T {
    scalaVersion().startsWith("3.")
  }

  def isScala213: T[Boolean] = T {
    scalaVersion().startsWith("2.13.")
  }

  def partialVersion(version: String): Option[(Int, Int)] = {
    val partial = version.split('.').take(2)
    for {
      major    <- partial.headOption
      majorInt <- major.toIntOption
      minor    <- partial.lastOption
      minorInt <- minor.toIntOption
    } yield (majorInt, minorInt)
  }

  def partialVersion: T[Option[(Int, Int)]] = T {
    partialVersion(scalaVersion())
  }

  def optimize: T[Boolean] = T(false)
  def scalacOptions: Target[Seq[String]] = T {
    val options = scalacOptions(scalaVersion(), optimize())
    super.scalacOptions() ++ options ++ additionalScalacOptions()
  }

  /// The location of user specific build properties. This is curremtly only setup to provide custom scalac options.
  /// This becomes useful when you want to temporarily enable a scalac option which is harder given mill runs a build serve/daemon.
  def userBuildProperties = T.source(T.workspace / "build.user.properties")

  def additionalScalacOptions = T {
    val propsPath = userBuildProperties().path
    if (os.exists(propsPath)) {
      try {
        val is = os.read.inputStream(propsPath)
        try {
          val props = new java.util.Properties()
          props.load(is)
          getAdditionalScalacOptions(props, partialVersion())
        } finally is.close()
      } catch {
        case e: Throwable =>
          println(s"Error reading $propsPath: ${e.getMessage}")
          Seq()
      }
    } else {
      Seq()
    }
  }

  def getAdditionalScalacOptions(props: Properties, partialVersion: Option[(Int, Int)]): Seq[String] = {
    val allProps =
      Option(props.getProperty("scalac.options.additional"))
        .map(str => str.split(' ').toSeq)
        .getOrElse(Seq.empty)
    partialVersion match {
      case None => allProps
      case Some((major, minor)) =>
        val majorProps =
          Option(props.getProperty(s"scalac.$major.x.options.additional"))
            .map(str => str.split(' ').toSeq)
            .getOrElse(Seq.empty)
        val majorMinorProps =
          Option(props.getProperty(s"scalac.$major.$minor.options.additional"))
            .map(str => str.split(" ").toSeq)
            .getOrElse(Seq.empty)
        allProps ++ majorProps ++ majorMinorProps
    }
  }

  def scalacOptions(scalaVersion: String, optimize: Boolean) = {

    val commonOptions = Seq("-deprecation", "-language:implicitConversions")

    val versionParts = scalaVersion.split("\\.")
    val extraOptions = versionParts match {
      case Array("2", _, _) =>
        Seq("-language:existentials", "-Yrangepos", "-Xsource:3", "-Xfatal-warnings")
      case Array("3", _, _) =>
        Seq("-Xignore-scala2-macros", "-Yretain-trees", "-Wvalue-discard")
      case _ =>
        Seq()
    }
    commonOptions ++ extraOptions
  }

  //  def compileIvyDeps = T{
  //    if(scalaVersion().startsWith("2.")) {
  //      super.compileIvyDeps() ++ Agg(
  //        com.github.ghik.`silencer-plugin`
  //      )
  //    } else {
  //      super.compileIvyDeps()
  //    }
  //  }
  //  def ivyDeps = T {
  //    if (scalaVersion().startsWith("2."))
  //      Agg(com.github.ghik.`silencer-lib`)
  //    else
  //      Agg()
  //  }
}
