package millbuild

import java.io.InputStream
import java.net.URI
import java.util.zip.{ZipEntry, ZipInputStream}

/**
 * Elm packages fetched at build time for the langkit's conformance corpus.
 *
 * These are third-party sources, so they are *input data* rather than repository content: the build downloads a pinned
 * commit, unpacks the `.elm` files into a task directory, and hands the path to the test that reads them. Nothing
 * lands in git, which keeps the licensing question where it belongs — the packages stay under their own licence, at a
 * revision recorded here.
 *
 * A pinned revision is also what makes the download cacheable and the corpus reproducible: Mill re-runs the task only
 * when this list changes.
 */
object ElmPackages {

  /**
   * A package to fetch.
   *
   * @param repository
   *   `owner/name` on GitHub
   * @param revision
   *   a tag or commit, pinned so the corpus does not change under us
   * @param licence
   *   the licence the sources carry, recorded because they are redistributed to nobody but still read by the build
   * @param why
   *   what this package covers that the others do not
   * @param shallow
   *   clone only the pinned revision, which is all the corpus reads. Set false only for a package whose history the
   *   build needs, which so far is none of them.
   */
  case class ElmPackage(
      repository: String,
      revision: String,
      licence: String,
      why: String,
      shallow: Boolean = true
  ) {
    def name: String   = repository.replace('/', '-')
    def cloneUrl: String = s"https://github.com/$repository.git"
    def zipUrl: String = s"https://codeload.github.com/$repository/zip/$revision"

    /** The tag or branch to clone, as `git` wants it. */
    def cloneRef: String = revision.stripPrefix("refs/tags/").stripPrefix("refs/heads/")
  }

  /**
   * The packages the corpus parses.
   *
   * `finos/morphir-elm` is Morphir's own and could have been vendored; it is fetched the same way so there is one
   * mechanism rather than two. The `elm/` packages carry constructs Morphir's Elm does not — effect modules, shader
   * blocks, and the operator declarations the fixity tables are built from.
   */
  val corpus: Seq[ElmPackage] = Seq(
    ElmPackage(
      repository = "elm/core",
      revision = "refs/tags/1.0.5",
      licence = "BSD-3-Clause",
      why = "the fixity declarations, and the widest range of ordinary Elm there is"
    ),
    ElmPackage(
      repository = "elm/html",
      revision = "refs/tags/1.0.0",
      licence = "BSD-3-Clause",
      why = "large modules of near-identical declarations, and Html.Lazy's higher-order types"
    ),
    ElmPackage(
      repository = "elm/browser",
      revision = "refs/tags/1.0.2",
      licence = "BSD-3-Clause",
      why = "effect modules with `where { command = …, subscription = … }`"
    ),
    ElmPackage(
      repository = "finos/morphir-elm",
      revision = "refs/tags/v2.99.0",
      licence = "Apache-2.0",
      why = "Morphir's own Elm, which is what this langkit exists to parse"
    )
  )

  /**
   * Fetch every package's `.elm` sources under `into`, one directory per package.
   *
   * A shallow `git clone` at the pinned tag is the first choice: it is the same mechanism a developer would reach for,
   * it works against any host `git` can reach, and it honours whatever proxy or credential configuration is already
   * in place. The zip endpoint is the fallback for an environment with no `git` on the path.
   *
   * Returns the directories written, in `corpus` order.
   */
  def fetchAll(into: os.Path): Seq[os.Path] =
    corpus.map { pkg =>
      val destination = into / pkg.name
      os.makeDir.all(destination)

      val fetched =
        attempt(cloneWith(ghCommand(pkg), pkg, destination)) ||
          attempt(cloneWith(gitCommand(pkg), pkg, destination)) ||
          attempt { unpackElmSources(pkg, destination); true }

      if (!fetched) sys.error(s"could not fetch ${pkg.repository} at ${pkg.revision} by gh, git, or zip")
      destination
    }

  private def attempt(fetch: => Boolean): Boolean =
    try fetch
    catch { case _: Exception => false }

  /**
   * `gh repo clone`, tried first for a GitHub repository.
   *
   * The GitHub CLI carries the user's authentication, so this is what reaches a private repository or a host behind
   * SSO, and what respects a token already configured for CI. Everything after it is a fallback for a machine without
   * `gh` installed or signed in.
   */
  private def ghCommand(pkg: ElmPackage): Seq[String] => Seq[String] =
    checkout => Seq("gh", "repo", "clone", pkg.repository, checkout.head, "--") ++ cloneFlags(pkg)

  /** Plain `git clone`, for a machine without `gh`, or a repository somewhere other than GitHub. */
  private def gitCommand(pkg: ElmPackage): Seq[String] => Seq[String] =
    checkout => Seq("git", "clone") ++ cloneFlags(pkg) ++ Seq(pkg.cloneUrl, checkout.head)

  /** Shallow by default: the corpus reads one revision and has no use for the history behind it. */
  private def cloneFlags(pkg: ElmPackage): Seq[String] =
    (if (pkg.shallow) Seq("--depth", "1") else Seq.empty) ++ Seq("--branch", pkg.cloneRef, "--quiet")

  /**
   * Run a clone command and copy out the `.elm` sources.
   *
   * Returns false rather than throwing when the tool is missing or the clone fails, so the caller can fall through to
   * the next mechanism.
   */
  private def cloneWith(
      command: Seq[String] => Seq[String],
      pkg: ElmPackage,
      destination: os.Path
  ): Boolean = {
    val checkout = destination / os.up / s"${pkg.name}-checkout"
    os.remove.all(checkout)

    val result = os.proc(command(Seq(checkout.toString))).call(check = false, stderr = os.Pipe, stdout = os.Pipe)

    if (result.exitCode != 0) {
      os.remove.all(checkout)
      false
    } else {
      try {
        os.walk(checkout)
          .filter(path => os.isFile(path) && path.ext == "elm" && path.segments.contains("src"))
          .foreach { source =>
            val target = destination / source.relativeTo(checkout)
            os.makeDir.all(target / os.up)
            os.copy.over(source, target)
          }
        true
      } finally os.remove.all(checkout)
    }
  }

  /** Stream the package's zip, writing out the `.elm` files under any `src` directory. */
  private def unpackElmSources(pkg: ElmPackage, destination: os.Path): Unit = {
    val connection = URI.create(pkg.zipUrl).toURL.openConnection()
    connection.setConnectTimeout(30000)
    connection.setReadTimeout(60000)

    val stream = new ZipInputStream(connection.getInputStream)
    try {
      Iterator
        .continually(stream.getNextEntry)
        .takeWhile(_ != null)
        .foreach { entry =>
          if (isElmSource(entry)) {
            // Drop the archive's top-level `<repo>-<revision>/` directory, which carries the revision in its name.
            val relative = entry.getName.split('/').drop(1).mkString("/")
            val target   = destination / os.SubPath(relative)
            os.makeDir.all(target / os.up)
            os.write.over(target, readEntry(stream))
          }
          stream.closeEntry()
        }
    } finally stream.close()
  }

  private def isElmSource(entry: ZipEntry): Boolean = {
    val path = entry.getName
    !entry.isDirectory && path.endsWith(".elm") && path.split('/').contains("src")
  }

  private def readEntry(stream: InputStream): Array[Byte] = {
    val buffer = new java.io.ByteArrayOutputStream()
    val chunk  = new Array[Byte](8192)
    Iterator
      .continually(stream.read(chunk))
      .takeWhile(_ > 0)
      .foreach(read => buffer.write(chunk, 0, read))
    buffer.toByteArray
  }
}
