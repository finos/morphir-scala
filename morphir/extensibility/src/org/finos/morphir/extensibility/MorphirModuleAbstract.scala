package org.finos.morphir.extensibility

abstract class MorphirModuleAbstract(val packageName: String, val moduleName: String) extends MorphirModuleLike {}

abstract class MorphirSdkModule(packageName: String, moduleName: String)
    extends MorphirModuleAbstract(packageName, moduleName) {}
