package millbuild

import mill.*, scalalib.*, scalafmt.*
import java.util.Properties
trait CommonCrossScalaModule extends ScalaModule with CommonScalaModule
    with ScalafmtModule { self => }

trait CommonScalaModule extends ScalaModule with CommonCoursierModule {
  def disableFatalWarnings = Task.Input {
    Task.env.get("DISABLE_WARNINGS_AS_ERRORS").map(_.toBoolean).getOrElse(false)
  }

  def isCIBuild = Task.Input {
    Task.env.get("CI").map(_.toBoolean).getOrElse(false)
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

  def partialVersion: T[Option[(Int, Int)]] = Task {
    partialVersion(scalaVersion())
  }

  def optimize: T[Boolean] = Task(false)

  def scalacOptions: T[Seq[String]] = Task {
    val options = scalacOptions(
      optimize = optimize(),
      isCIBuild = isCIBuild(),
      disableFatalWarnings = disableFatalWarnings()
    )
    super.scalacOptions() ++ options ++ additionalScalacOptions()
  }

  def scalaDocOptions = Task {
    filterScala3DocOptions(super.scalaDocOptions())
  }

  /// The location of user specific build properties. This is curremtly only setup to provide custom scalac options.
  /// This becomes useful when you want to temporarily enable a scalac option which is harder given mill runs a build serve/daemon.
  def userBuildProperties = Task.Source(mill.api.BuildCtx.workspaceRoot / "build.user.properties")

  def additionalScalacOptions = Task {
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
      case None                 => allProps
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

  lazy val commonCompilerOptions = Seq(
    "-Xkind-projector",
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8",                  // Specify character encoding used by source files.
    "-feature",               // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked"                     // Enable additional warnings where generated code depends on assumptions.
  )

  def targetScalacOptions = Seq("-release", "25")

  def scalacOptions(optimize: Boolean, isCIBuild: Boolean, disableFatalWarnings: Boolean) = {
    val options = commonCompilerOptions ++ Seq(
      // TODO: Enable later
      // "-source:3.0-migration",
      "-explain",
      "-explain-types",
      "-Xignore-scala2-macros",
      "-Yretain-trees",
      "-Wvalue-discard",
      // Suppress warnings for generated code and migration issues
      "-Wconf:msg=Ignoring.*this.*qualifier:s",                                    // BuildInfo private[this]
      "-Wconf:msg=.*is deprecated for wildcard arguments.*:s",                     // _ -> ? migration
      "-Wconf:msg=.*will be duplicated at each inline site.*:s",                   // inline anonymous classes
      "-Wconf:msg=.*has been deprecated.*uninitialized.*:s",                       // = _ -> = uninitialized
      "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s", // implicit -> using migration
      "-Wconf:msg=with as a type operator has been deprecated.*:s",                // with -> & migration
      "-Wconf:msg=The syntax.*_\\*.*is no longer supported.*:s",                   // x: _* -> x* migration
      "-Wconf:msg=.*is not declared infix.*:s"                                     // infix type notation
    )

    val optionsWithTarget = options ++ targetScalacOptions

    // Warnings as errors are always enabled for the CI build
    // and can be disabled by setting the DISABLE_WARNINGS_AS_ERRORS environment variable to true
    if (isCIBuild || !disableFatalWarnings)
      optionsWithTarget ++ Seq("-Werror")
    else
      optionsWithTarget
  }

  def filterScala3DocOptions(opts: Seq[String]) =
    opts.filterNot(_.startsWith("-Xfatal"))
      .filterNot(_.startsWith("-Ywarn"))
      .filterNot(_.startsWith("-W"))
}
