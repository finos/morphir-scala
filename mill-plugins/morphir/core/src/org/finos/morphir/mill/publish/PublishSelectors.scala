package org.finos.morphir.mill.publish

/**
 * Kind-split filters over Mill module paths resolved from `__.publishSonatypeCentral`.
 *
 * `ci/MorphirCi.mill` supplies the resolved names; this object is the pure selection logic.
 */
object PublishSelectors {

  def libraryModules(
      allPublishModules: Seq[String],
      libraryModulePrefix: String,
      libraryExcludedPrefixes: Seq[String],
      pluginRoots: Seq[String]
  ): Seq[String] = {
    if libraryModulePrefix.isEmpty then
      throw new IllegalArgumentException(
        "ci.libraryModulePrefix is empty; set it in ci/package.mill.yaml"
      )
    allPublishModules
      .filter(_.startsWith(libraryModulePrefix))
      .filterNot(m => libraryExcludedPrefixes.exists(m.startsWith))
      .filterNot(isUnderPluginRoot(_, pluginRoots))
      .distinct
      .sorted
  }

  def dropExcluded(modules: Seq[String], excludedModuleSubstrings: Seq[String]): Seq[String] =
    modules.filterNot(m => excludedModuleSubstrings.exists(m.contains)).distinct.sorted

  def allKinds(libraries: Seq[String], plugins: Seq[String]): Seq[String] = (libraries ++ plugins).distinct.sorted

  def isUnderPluginRoot(module: String, pluginRoots: Seq[String]): Boolean =
    pluginRoots.exists(root => module == root || module.startsWith(root + "."))
}
