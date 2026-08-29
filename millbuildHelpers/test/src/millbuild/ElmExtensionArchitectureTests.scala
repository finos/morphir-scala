package millbuild

import utest.*

object ElmExtensionArchitectureTests extends TestSuite:
  val tests = Tests:
    test("reports forbidden source stacks and missing Kyo module dependencies"):
      val violations = ElmExtensionArchitecture.violations(
        Seq(
          ElmExtensionArchitecture.Source("compiler.scala", "import org.finos.morphir.ir.Type"),
          ElmExtensionArchitecture.Source("protocol.scala", "import zio.json.ast.Json")
        ),
        "moduleDeps: [build.morphir.jvm]",
        "moduleDeps: [build.morphir.jvm]",
        "moduleDeps: [build.morphir.interop.zio.json.jvm]\nmvnDeps: [dev.zio::zio-json]"
      )
      assert(violations.size == 11)
      assert(violations.exists(_.contains("compiler.scala contains org.finos.morphir.ir")))
      assert(violations.exists(_.contains("protocol.scala contains zio.json")))
      assert(violations.exists(_.contains("compiler manifest must contain build.morphir.model.jvm")))
      assert(violations.exists(_.contains("v3 compatibility manifest must contain build.morphir.model.jvm")))
      assert(violations.exists(_.contains("MEP manifest must contain io.getkyo::kyo-jsonrpc")))

    test("accepts a Kyo-native compiler and MEP module"):
      val violations = ElmExtensionArchitecture.violations(
        Seq(ElmExtensionArchitecture.Source("compiler.scala", "import kyo.{Chunk, Result}")),
        "moduleDeps: [build.morphir.model.jvm]",
        "moduleDeps: [build.morphir.model.jvm]",
        "extends: [build.MorphirKyoSchemaJsonMvnDeps]\nmoduleDeps: [build.morphir.model.compat.v3.jvm]\nmvnDeps: [io.getkyo::kyo-jsonrpc]"
      )
      assert(violations.isEmpty)
