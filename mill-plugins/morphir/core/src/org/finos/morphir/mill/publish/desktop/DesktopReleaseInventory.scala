package org.finos.morphir.mill.publish.desktop

/** The files a single platform contributed to a staged release, after classification. */
final case class PlatformAssets(
    platform: DesktopPlatform,
    archive: String,
    installers: Seq[String],
    ignored: Seq[String]
)

/**
 * Classifies raw electron-builder output by extension.
 *
 * Matching on extension rather than on an expected filename keeps this independent of electron-builder's naming
 * templates, which differ per target (`deb` in particular imposes its own). Files with an unknown extension —
 * `.blockmap`, `latest-mac.yml` — are reported as ignored, not rejected.
 */
object DesktopReleaseInventory {
  def classify(platform: DesktopPlatform, fileNames: Seq[String]): Either[String, PlatformAssets] = {
    val sorted = fileNames.sorted

    // Longest extension first, so `app.tar.gz` is claimed by `tar.gz` and never by a shorter suffix.
    val byLength = platform.allExts.sortBy(-_.length)

    def matches(name: String, ext: String): Boolean = name.endsWith(s".$ext")

    def claim(ext: String): Either[String, String] = {
      val hits = sorted.filter { name =>
        matches(name, ext) && byLength.find(matches(name, _)).contains(ext)
      }
      hits match {
        case Seq(one) => Right(one)
        case Seq()    => Left(s"${platform.token}: no .$ext file staged")
        case many     => Left(s"${platform.token}: expected one .$ext file, found ${many.mkString(", ")}")
      }
    }

    for {
      archive    <- claim(platform.archive.ext)
      installers <- platform.installerExts
        .foldLeft[Either[String, Seq[String]]](Right(Seq.empty)) { (accumulated, ext) =>
          for {
            already <- accumulated
            hit     <- claim(ext)
          } yield already :+ hit
        }
    } yield {
      val claimed = (archive +: installers).toSet
      PlatformAssets(platform, archive, installers, sorted.filterNot(claimed.contains))
    }
  }
}
