package millbuild

/**
 * Pure partitioning of resolved JS/Wasm task selectors between the desktop/UI CI job and the
 * platform CI job, for `ci/MorphirCi.mill`'s `testJs` and `testJsWasmLink`.
 *
 * The `test-js` job used to link every JS and Wasm module in one Mill daemon. `morphir.ui`,
 * `morphir.desktop` and `morphir.appkit.electron` together with the rest of the tree pushed the
 * Scala.js linker past an 8 GB heap and it died with `OutOfMemoryError` inside the source-map
 * printer (morphir-oyn). Splitting the desktop/UI link load into its own CI job spreads the two
 * peaks across two runners with two daemons.
 *
 * Mill wildcards cannot subtract a subset, so `ci/MorphirCi.mill` resolves the full wildcard first,
 * the same way `ci.lint` does, and this object partitions the *resolved* selector strings by which
 * module they belong to — mirroring how [[LintSelectors]] filters `ci.lint`'s resolved module list.
 *
 * `Seq.partition` makes the split exhaustive and disjoint by construction: every resolved selector
 * lands in exactly one of the two buckets, and the buckets' union is the input, unchanged. What
 * still needs checking is that [[desktopModuleRoots]] — a fixed list — actually matches something
 * real: a stale or misspelled root would silently starve the desktop job of every target, and an
 * empty test job that does nothing looks exactly like one that passed. `selectGroup` fails loudly
 * on an empty bucket rather than let that happen.
 */
object JsTestSelectors {

  /** `test-js`'s "Run JS tests" step: compile, publish, and both JS and Wasm test runs. */
  val compileSelector: String  = "morphir.__.js.__.compile"
  val publishSelector: String  = "morphir.__.js.publishArtifacts"
  val jsTestSelector: String   = "morphir.__.js.__.test"
  val wasmTestSelector: String = "morphir.__.wasm.__.test"

  /** The four wildcards evaluated together in the "Run JS tests" step. */
  val mainPhaseSelectors: Seq[String] =
    Seq(compileSelector, publishSelector, jsTestSelector, wasmTestSelector)

  /** `test-js`'s "Link the WebAssembly variants" step — run at `-j 1` regardless of group. */
  val wasmLinkSelector: String = "morphir.__.wasm.fullLinkJS"

  /** Module roots moved into the desktop/UI CI job — see morphir-oyn. */
  val desktopModuleRoots: Seq[String] = Seq("morphir.ui", "morphir.desktop", "morphir.appkit.electron")

  private def segments(path: String): Seq[String] = path.split('.').toIndexedSeq

  /**
   * True when `resolved` names a task under one of [[desktopModuleRoots]] — a dotted-segment
   * prefix match, not a plain substring one, so `morphir.appkit.js.compile` does not match the
   * `morphir.appkit.electron` root and a hypothetical `morphir.uiThing` module would not match
   * `morphir.ui`.
   */
  def isDesktopTask(resolved: String): Boolean = {
    val resolvedSegments = segments(resolved)
    desktopModuleRoots.exists { root =>
      val rootSegments = segments(root)
      resolvedSegments.size > rootSegments.size && resolvedSegments.startsWith(rootSegments)
    }
  }

  /** Splits resolved task selectors into (desktop, platform) — exhaustive and disjoint by construction. */
  def partition(resolved: Seq[String]): (desktop: Seq[String], platform: Seq[String]) = {
    val (desktop, platform) = resolved.partition(isDesktopTask)
    (desktop = desktop, platform = platform)
  }

  /**
   * The selectors belonging to `group` (`"desktop"` or `"platform"`), or an error — an unknown
   * group name, or a bucket that came back empty (which would otherwise let a CI job run zero
   * targets and still report success).
   */
  def selectGroup(resolved: Seq[String], group: String, what: String): Either[String, Seq[String]] = {
    val split = partition(resolved)
    val kept = group match {
      case "desktop"  => Right(split.desktop)
      case "platform" => Right(split.platform)
      case other      => Left(s"$what: unknown group '$other' (expected 'platform' or 'desktop')")
    }
    kept.flatMap { selectors =>
      if selectors.isEmpty then Left(s"$what: no targets remain for group '$group'")
      else Right(selectors)
    }
  }
}
