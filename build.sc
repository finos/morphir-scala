import $meta._
import $ivy.`de.tototec::de.tobiasroeser.mill.integrationtest::0.7.1`
import $ivy.`io.chris-kipp::mill-ci-release::0.1.9`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $file.project.deps, deps.{Deps, MillVersions, ScalaVersions, Versions => Vers}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import de.tobiasroeser.mill.integrationtest._
import io.kipp.mill.ci.release.CiReleaseModule
import millbuild._
import millbuild.crossplatform._
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._, scalafmt._

/**
 * The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code for
 * other Scala versions. We may also directly cross-compile to additional Scla versions.
 */
val morphirScalaVersion: String = ScalaVersions.scala3x

val docsScalaVersion: String = ScalaVersions.scala213 //This really should match but need to figure it out

import mill.eval.{Evaluator, EvaluatorPaths}
// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}

trait MorphirPublishModule extends CiReleaseModule with JavaModule {
  import mill.scalalib.publish._
  def packageDescription: String = s"The $artifactName package"

  def pomSettings = PomSettings(
    description = packageDescription,
    organization = "org.finos.morphir",
    url = "https://github.com/finos/morphir-scala",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("finos", "morphir-scala"),
    developers = Seq(
      Developer("DamianReeves", "Damian Reeves", "https://github.com/damianreeves")
    )
  )
}

object morphir extends Cross[MorphirModule](ScalaVersions.all) {
  object build extends Module {
    object integration extends Module {
      object `mill-morphir-elm` extends Cross[MillMorphirElmPlugin](MillVersions.all)
      trait MillMorphirElmPlugin
          extends Cross.Module[String]
          with ScalaModule
          with ScalafmtModule
          with MorphirPublishModule {

        def millVersion = crossValue

        val pluginName = "mill-morphir-elm"

        def scalaVersion          = ScalaVersions.millScalaVersion
        override def artifactName = s"${pluginName}_mill${MillVersions.millBinaryVersion(millVersion)}"
        override def compileIvyDeps = super.compileIvyDeps() ++ Agg(
          ivy"com.lihaoyi::mill-scalalib:${millVersion}"
        )
        override def scalacOptions = Seq("-Ywarn-unused", "-deprecation")
        override def sources = T.sources {
          super.sources() ++ Seq(
            millSourcePath / s"src-mill${millVersion.split('.').take(2).mkString(".")}"
          ).map(PathRef(_))
        }
      }
      trait ItestCross extends MillIntegrationTestModule with Cross.Module[String]
    }
  }
}
trait MorphirModule extends Cross.Module[String] { morphir =>
  val workspaceDir = millbuild.build.millSourcePath

  trait MorphirCommonModule extends CrossPlatformScalaModule with CrossValue with CommonCrossScalaModule {
    def semanticDbVersion = T.input(Vers.semanticDb(partialVersion()))
    def compilerPluginDependencies(selectedScalaVersion: String) =
      Agg.when(selectedScalaVersion.startsWith("3.")) {
        Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
      }
  }

  trait MorphirJVMModule extends MorphirCommonModule {
    def platform = Platform.JVM
  }

  trait MorphirJSModule extends MorphirCommonModule with ScalaJSModule {
    import mill.scalajslib.api._
    def platform       = Platform.JS
    def scalaJSVersion = ScalaVersions.scalaJSVersion
  }

  trait MorphirNativeModule extends MorphirCommonModule with ScalaNativeModule {
    def platform           = Platform.Native
    def scalaNativeVersion = ScalaVersions.scalaNativeVersion
  }

  object contrib extends Module {
    object knowledge extends CrossPlatform with CrossValue {
      def enableNative(module: Module): Boolean = crossValue.startsWith("2.13.")
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def ivyDeps = Agg(
          Deps.com.lihaoyi.sourcecode,
          Deps.dev.zio.`zio-streams`
        )
        def platformSpecificModuleDeps = Seq(toolkit.core)
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(
            Deps.dev.zio.zio,
            Deps.dev.zio.`zio-streams`,
            Deps.com.eed3si9n.expecty.expecty,
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`
          )
          def moduleDeps = super.moduleDeps ++ Seq(testing.munit.jvm, testing.munit.zio.jvm)
        }
      }

      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.Munit {
          def ivyDeps = Agg(
            Deps.dev.zio.zio,
            Deps.dev.zio.`zio-streams`,
            Deps.com.eed3si9n.expecty.expecty,
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`
          )
          def moduleDeps = super.moduleDeps ++ Seq(testing.munit.js, testing.munit.zio.js)
        }
      }

      object native extends Shared with MorphirNativeModule {
        // NOTE: Issues arise when trying to run tests on native.  Need to figure out how to get this working

        // object test extends ScalaNativeTests with TestModule.ZioTest {
        //   def ivyDeps = Agg(
        //     Deps.dev.zio.zio,
        //     Deps.dev.zio.`zio-streams`,
        //     Deps.com.eed3si9n.expecty.expecty,
        //     Deps.org.scalameta.munit,
        //     Deps.org.scalameta.`munit-scalacheck`
        //   )
        //   def moduleDeps = super.moduleDeps ++ Seq(testing.munit.native, testing.munit.zio.native)
        // }
      }
    }
  }

  object core extends CrossPlatform with CrossValue{
    trait Shared extends MorphirCommonModule with MorphirPublishModule {
      def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.com.beachape.enumeratum,
        Deps.com.lihaoyi.castor,
        Deps.com.lihaoyi.pprint,
        Deps.com.lihaoyi.`upickle`,
        Deps.com.outr.scribe,
        Deps.org.typelevel.`paiges-core`
      )

      def platformSpecificModuleDeps = Seq(morphir.foundations)
    }

    object jvm extends Shared with MorphirJVMModule {
      object test extends ScalaTests with TestModule.ZioTest {
        def moduleDeps = super.moduleDeps ++ Seq(testing.zio.jvm)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaJSTests with TestModule.ZioTest {        
        def moduleDeps = super.moduleDeps ++ Seq(testing.zio.js)
      }
    }

    object native extends Shared with MorphirNativeModule {
      object test extends ScalaNativeTests with TestModule.ZioTest {
        def moduleDeps = super.moduleDeps ++ Seq(testing.zio.native)
      }
    }

  }

  object datamodel extends CrossPlatform {
    trait Shared extends MorphirCommonModule with MorphirPublishModule {
      def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.com.lihaoyi.geny,
        Deps.com.lihaoyi.sourcecode,
        Deps.com.lihaoyi.pprint
      )
      def platformSpecificModuleDeps = Seq(datamodel.macros)
    }

    object jvm extends Shared with MorphirJVMModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }

    object native extends Shared with MorphirNativeModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }

    object json extends Module {
      object zio extends Module {
        object jvm extends MorphirJVMModule with MorphirPublishModule {

          def ivyDeps    = Agg(Deps.dev.zio.`zio-json`)
          def moduleDeps = Seq(datamodel.jvm)

          object test extends ScalaTests with TestModule.Munit {
            def ivyDeps: T[Agg[Dep]] = Agg(
              Deps.org.scalameta.munit,
              Deps.org.scalameta.`munit-scalacheck`
            )
          }
        }

        object js extends MorphirJSModule with MorphirPublishModule {

          def ivyDeps    = Agg(Deps.dev.zio.`zio-json`)
          def moduleDeps = Seq(datamodel.js)

          object test extends ScalaTests with TestModule.Munit {
            def ivyDeps: T[Agg[Dep]] = Agg(
              Deps.org.scalameta.munit,
              Deps.org.scalameta.`munit-scalacheck`
            )
          }
        }
      }
    }

    object macros extends CrossPlatform {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def compileIvyDeps = T {
          super.compileIvyDeps() ++ Agg.when(scalaVersion().startsWith("2."))(
            Deps.org.`scala-lang`.`scala-reflect`(scalaVersion()),
            Deps.org.`scala-lang`.`scala-compiler`(scalaVersion())
          )
        }

        def scalacOptions = T {
          super.scalacOptions().concatIf(isScala213())("-language:experimental.macros")
        }
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        }
      }
      object js extends Shared with MorphirJSModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        }
      }
      object native extends Shared with MorphirNativeModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        }
      }
    }
  }

  object elm extends Module {
    object facade extends CrossPlatform with CrossValue {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {    
        
        def platformSpecificModuleDeps = Seq(morphir.foundations)
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = super.ivyDeps() ++ Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        
        }
      }

      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.Munit {
          def ivyDeps = super.ivyDeps() ++ Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        }
      }

      object native extends Shared with MorphirNativeModule {
        object test extends ScalaNativeTests with TestModule.Munit {
          def ivyDeps = super.ivyDeps() ++ Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        }
      }            
    }  
  }

  object foundations extends CrossPlatform with CrossValue {
    trait Shared extends MorphirCommonModule with MorphirPublishModule {
      def compileIvyDeps = super.compileIvyDeps() ++ (if (crossScalaVersion.startsWith("2."))
                                                        Agg(
                                                          Deps.org.`scala-lang`.`scala-reflect`(crossScalaVersion),
                                                          Deps.org.`scala-lang`.`scala-compiler`(crossScalaVersion)
                                                        )
                                                      else Agg.empty)

      def scalacOptions = T {
        // val additionalOptions = if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
        val additionalOptions =
          if (crossScalaVersion.startsWith("2.13")) Seq("-language:experimental.macros") else Seq.empty
        super.scalacOptions() ++ additionalOptions
      }          
      def platformSpecificModuleDeps = Seq(foundations.macros)
    }
    object jvm extends Shared with MorphirJVMModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.munit.jvm)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaJSTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.munit.js)
      }
    }

    object native extends Shared with MorphirNativeModule {
      object test extends ScalaNativeTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.munit.native)
      }
    }

    object macros extends CrossPlatform {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def compileIvyDeps = T {
          super.compileIvyDeps() ++ Agg.when(scalaVersion().startsWith("2."))(
            Deps.org.`scala-lang`.`scala-reflect`(scalaVersion()),
            Deps.org.`scala-lang`.`scala-compiler`(scalaVersion())
          )
        }

        def scalacOptions = T {
          super.scalacOptions().concatIf(isScala213())("-language:experimental.macros")
        }
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
          def moduleDeps = super.moduleDeps ++ Agg(testing.munit.jvm)
        }
      }
      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
          def moduleDeps = super.moduleDeps ++ Agg(testing.munit.js)
        }
      }
      object native extends Shared with MorphirNativeModule {
        object test extends ScalaNativeTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
          def moduleDeps = super.moduleDeps ++ Agg(testing.munit.native)
        }
      }
    }

    object platform extends Module {
      object services extends CrossPlatform with CrossValue {
        trait Shared extends MorphirCommonModule with MorphirPublishModule {
          def platformSpecificModuleDeps = Seq(foundations)
        }

        object jvm extends Shared with MorphirJVMModule {
          object test extends ScalaTests with TestModule.Munit {
            def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
            def moduleDeps = super.moduleDeps ++ Agg(testing.munit.jvm)
          }
        }

        object js extends Shared with MorphirJSModule {
          object test extends ScalaJSTests with TestModule.Munit {
            def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
            def moduleDeps = super.moduleDeps ++ Agg(testing.munit.js)
          }
        }

        object native extends Shared with MorphirNativeModule {
          object test extends ScalaNativeTests with TestModule.Munit {
            def ivyDeps = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
            def moduleDeps = super.moduleDeps ++ Agg(testing.munit.native)
          }
        }
      }
    }
  }
  
  object lib extends Module {

    object interop extends CrossPlatform {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {}
      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit)
        }
      }
      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit)
        }
      }
      object native extends Shared with MorphirNativeModule {
        object test extends ScalaNativeTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit)
        }
      }
    }
  }

  object runtime extends CrossPlatform with CrossValue {

    def enableNative(module: Module): Boolean = crossValue.startsWith("2.13.")
    trait Shared extends MorphirCommonModule with MorphirPublishModule {
      def platformSpecificModuleDeps = Seq(datamodel, toolkit.core, toolkit.codec.zio.json)
    }

    object jvm extends Shared with MorphirJVMModule {
      object test extends ScalaTests with TestModule.ZioTest {
        def ivyDeps    = Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.zio.jvm)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaJSTests with TestModule.ZioTest {
        def ivyDeps    = Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.zio.js)
      }
    }

    object native extends Shared with MorphirNativeModule {
      object test extends ScalaNativeTests with TestModule.ZioTest {
        def ivyDeps    = Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.zio.native)
      }
    }

    object zio extends CrossPlatform with CrossValue {
      def enableNative(module: Module): Boolean = crossValue.startsWith("2.13.")
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-prelude`)
        def platformSpecificModuleDeps = Seq(datamodel, runtime, toolkit.core)
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps    = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
          def moduleDeps = super.moduleDeps ++ Agg(
            testing.munit.jvm, 
            testing.munit.zio.jvm
          )
        }
      }

      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.Munit {
          def ivyDeps    = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
          def moduleDeps = super.moduleDeps ++ Agg(testing.munit.js, testing.munit.zio.js)
        }
      }

      object native extends Shared with MorphirNativeModule {
        object test extends ScalaNativeTests with TestModule.Munit {
          def ivyDeps    = Agg(Deps.org.scalameta.munit, Deps.org.scalameta.`munit-scalacheck`)
          def moduleDeps = super.moduleDeps ++ Agg(testing.munit.native, testing.munit.zio.native)
        }
      }
    }
  }

  object testing extends Module {
    object munit extends CrossPlatform {
      trait Shared extends MorphirCommonModule {
        def ivyDeps = Agg(
          ivy"io.github.cquiroz::scala-java-time::2.5.0",
          Deps.com.eed3si9n.expecty.expecty,
          Deps.org.scalameta.munit,
          Deps.org.scalameta.`munit-scalacheck`
        )

        def platformSpecificModuleDeps = Seq(testing.munit.macros)
        def scalacOptions = T {
            super.scalacOptions().concatIf(isScala213())("-language:experimental.macros")
          }
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit
      }
      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.Munit
      }
      object native extends Shared with MorphirNativeModule {
        object test extends ScalaNativeTests with TestModule.Munit
      }

      object macros extends CrossPlatform with CrossValue {
        trait Shared extends MorphirCommonModule {
          def compileIvyDeps = T {
            super.compileIvyDeps() ++ Agg.when(scalaVersion().startsWith("2."))(
              Deps.org.`scala-lang`.`scala-reflect`(scalaVersion()),
              Deps.org.`scala-lang`.`scala-compiler`(scalaVersion())
            )
          }

          def ivyDeps = super.ivyDeps() ++ Agg(
            ivy"io.github.cquiroz::scala-java-time::2.5.0",
            Deps.com.eed3si9n.expecty.expecty,
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`
          )

          def scalacOptions = T {
            super.scalacOptions().concatIf(isScala213())("-language:experimental.macros")
          }
        }

        object jvm extends Shared with MorphirJVMModule
        object js extends Shared with MorphirJSModule 
        object native extends Shared with MorphirNativeModule
      }

      object zio extends CrossPlatform {
        trait Shared extends MorphirCommonModule {
          def ivyDeps = super.ivyDeps() ++ Agg(
            ivy"io.github.cquiroz::scala-java-time::2.5.0",
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`,
            Deps.dev.zio.zio
          )
          def platformSpecificModuleDeps = Seq(munit)
        }

        object jvm extends Shared with MorphirJVMModule {
          object test extends ScalaTests with TestModule.Munit
        }
        object js extends Shared with MorphirJSModule {
          object test extends ScalaJSTests with TestModule.Munit
        }
        object native extends Shared with MorphirNativeModule {
          object test extends ScalaNativeTests with TestModule.Munit
        }
      }
    }

    object zio extends CrossPlatform {
      trait Shared extends MorphirCommonModule {
        def ivyDeps = Agg(
          ivy"io.github.cquiroz::scala-java-time::2.5.0",
          Deps.dev.zio.`zio-test`,
          Deps.dev.zio.`zio-test-sbt`)
      }
      object jvm    extends Shared with MorphirJVMModule
      object js     extends Shared with MorphirJSModule
      object native extends Shared with MorphirNativeModule
    }
  }

  object toolkit extends Module {

    object codec extends Module {
      object zio extends Module {
        object json extends CrossPlatform with CrossValue {
          trait Shared extends MorphirCommonModule with MorphirPublishModule {
            def ivyDeps                    = Agg(Deps.dev.zio.`zio-json`)
            def platformSpecificModuleDeps = Seq(core)
          }
          object jvm extends Shared with MorphirJVMModule {
            object test extends ScalaTests with TestModule.ZioTest {
              def ivyDeps =
                super.ivyDeps() ++ Agg(
                  Deps.dev.zio.`zio-json-golden`,
                  ivy"io.github.deblockt:json-diff:0.0.5",
                  Deps.dev.zio.`zio-process`
                )

              def moduleDeps = super.moduleDeps ++ Agg(core.testing.jvm, morphir.testing.zio.jvm)
            }
          }
          object js extends Shared with MorphirJSModule {
            // object test extends ScalaTests with TestModule.ZioTest {
            //   def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
            //   def moduleDeps = super.moduleDeps ++ Agg(testing.js, morphir.testing.zio.js)
            // }
          }
        }
      }
    }

    object core extends CrossPlatform with CrossValue {
      def enableNative(module: Module): Boolean = crossValue.startsWith("2.13.")

      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def ivyDeps = Agg(
          Deps.com.lihaoyi.sourcecode,
          Deps.dev.zio.zio,
          Deps.dev.zio.`zio-prelude`,
          Deps.com.lihaoyi.pprint,
          Deps.org.typelevel.`paiges-core`
        ) ++ Agg.when(!platform.isNative)(Deps.io.lemonlabs.`scala-uri`)

        def platformSpecificModuleDeps = Seq(datamodel, lib.interop)
      }
      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.ZioTest {
          def ivyDeps    = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
          def moduleDeps = super.moduleDeps ++ Agg(testing.jvm, morphir.testing.zio.jvm)
        }
      }
      object js extends Shared with MorphirJSModule {
        // object test extends ScalaTests with TestModule.ZioTest {
        //   def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        //   def moduleDeps = super.moduleDeps ++ Agg(testing.js, morphir.testing.zio.js)
        // }

      }
      object native extends Shared with MorphirNativeModule {
        // object test extends ScalaTests with TestModule.ZioTest {
        //   def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        //   def moduleDeps = super.moduleDeps ++ Agg(testing.native, morphir.testing.zio.native)
        // }
      }

      object testing extends CrossPlatform {
        trait Shared extends MorphirCommonModule {
          def ivyDeps                    = Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-magnolia`)
          def platformSpecificModuleDeps = Seq(morphir.testing.zio, toolkit.core)

        }
        object jvm extends Shared with MorphirJVMModule {
          object test extends ScalaTests with TestModule.ZioTest {
            def ivyDeps    = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
            def moduleDeps = super.moduleDeps ++ Agg(morphir.testing.zio.jvm)
          }
        }
        object js extends Shared with MorphirJSModule {
          // object test extends ScalaTests with TestModule.ZioTest {
          //   def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
          //   def moduleDeps = super.moduleDeps ++ Agg(morphir.testing.zio.js)
          // }
        }
        // TODO: We make lots of use of `zio-test-magnolia` until that is supported on native we can't test on native
        // object native extends Shared with MorphirNativeModule {
        //   object test extends ScalaTests with TestModule.ZioTest {
        //     def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        //     def moduleDeps = super.moduleDeps ++ Agg(morphir.testing.zio.native)
        //   }
        // }
      }
    }

    object util extends CrossPlatform {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def ivyDeps = Agg(Deps.dev.zio.`izumi-reflect`)
      }

      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`,
            Deps.com.eed3si9n.expecty.expecty
          )
        }
      }

      object js extends Shared with MorphirJSModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`,
            Deps.com.eed3si9n.expecty.expecty
          )
        }
      }

      object native extends Shared with MorphirNativeModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(
            Deps.org.scalameta.munit,
            Deps.org.scalameta.`munit-scalacheck`,
            Deps.com.eed3si9n.expecty.expecty
          )
        }
      }
    }
  }

  object vfile extends CrossPlatform with CrossValue {
    def enableNative(module: Module) = !crossValue.startsWith("3")
    trait Shared extends MorphirCommonModule with MorphirPublishModule {
      def ivyDeps = Agg(
        Deps.com.lihaoyi.sourcecode,
        Deps.com.lihaoyi.geny,
        Deps.com.lihaoyi.pprint,
        Deps.org.typelevel.`paiges-core`
      )

      def platformSpecificModuleDeps = Seq(morphir.toolkit.util)
    }

    object jvm extends Shared with MorphirJVMModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }

    object native extends Shared with MorphirNativeModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }
  }

}

object site extends Docusaurus2Module with MDocModule {
  val workspaceDir = millbuild.build.millSourcePath

  override def scalaMdocVersion: T[String] = T("2.3.7")
  override def scalaVersion                = T(docsScalaVersion)
  // MD Sources that must be compiled with Scala MDoc
  override def mdocSources = T.sources(workspaceDir / "docs")
  // MD Sources that are just plain MD files
  override def docusaurusSources = T.sources(workspaceDir / "website")

  override def watchedMDocsDestination: T[Option[os.Path]] = T(Some(docusaurusBuild().path / "docs"))
  override def compiledMdocs: Sources                      = T.sources(mdoc().path)
  object test extends ScalaTests with TestModule.Munit {}
}
