package org.finos.morphir.mill.javascript

object NeutralJavaScriptConsumer {
  def usesOnlyContracts(
      runtime: JavaScriptRuntimeModule,
      packages: JavaScriptPackageManagerModule
  ): Boolean = packages.runtime.eq(runtime)
}
