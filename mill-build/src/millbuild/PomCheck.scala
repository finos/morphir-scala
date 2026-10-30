package millbuild

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Element, Node}

/**
 * Static checks on a generated POM that Maven Central's validation depends on but Mill's upload does not report.
 *
 * From 0.5.0-M05 to 0.5.0-M10 every Scala.js and Scala Native artifact whose YAML named a versionless suite dependency
 * uploaded with "Successfully published" and never reached Central: its POM named the JVM artifact (`kyo-config_3`)
 * with no version, and the `dependencyManagement` pin was keyed by the platformed name. These checks catch that shape
 * before an upload instead of after a release.
 */
object PomCheck {

  enum Platform(val scalaSuffix: Option[String]) {
    case Jvm    extends Platform(None)
    case Js     extends Platform(Some("_sjs1_3"))
    case Native extends Platform(Some("_native0.5_3"))
  }

  object Platform {

    /** The platform of a Mill module path: `morphir.ui.js` is Scala.js, `morphir.ui.native` Scala Native. */
    def of(modulePath: String): Platform =
      modulePath.split('.').lastOption match {
        case Some("js")     => Js
        case Some("native") => Native
        case _              => Jvm
      }
  }

  final case class Coordinate(groupId: String, artifactId: String, version: Option[String]) {
    def render: String = s"$groupId:$artifactId"
  }

  /** Every problem in `pom`, as one readable line each; empty when the POM is publishable. */
  def problems(pom: String, platform: Platform): Seq[String] = {
    val project    = parse(pom)
    val dependencies = childElements(project, "dependencies").flatMap(coordinates)
    val managed = childElements(project, "dependencyManagement")
      .flatMap(childElements(_, "dependencies"))
      .flatMap(coordinates)
      .filter(_.version.isDefined)
      .map(c => (c.groupId, c.artifactId))
      .toSet

    val unpinned = dependencies.collect {
      case c if c.version.isEmpty && !managed.contains((c.groupId, c.artifactId)) =>
        s"${c.render} has no version and no dependencyManagement pin"
    }
    val unplatformed = platform.scalaSuffix.toSeq.flatMap { suffix =>
      dependencies.collect {
        case c if c.artifactId.endsWith("_3") && !c.artifactId.endsWith(suffix) =>
          s"${c.render} is a JVM artifact; expected the ${suffix.drop(1)} variant"
      }
    }
    unpinned ++ unplatformed
  }

  private def parse(pom: String): Element = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(false)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.newDocumentBuilder()
      .parse(new ByteArrayInputStream(pom.getBytes(StandardCharsets.UTF_8)))
      .getDocumentElement
  }

  private def coordinates(dependencies: Element): Seq[Coordinate] =
    childElements(dependencies, "dependency").map { dependency =>
      Coordinate(
        text(dependency, "groupId").getOrElse(""),
        text(dependency, "artifactId").getOrElse(""),
        text(dependency, "version").filter(_.nonEmpty)
      )
    }

  private def text(parent: Element, name: String): Option[String] =
    childElements(parent, name).headOption.map(_.getTextContent.trim)

  private def childElements(parent: Element, name: String): Seq[Element] = {
    val children = parent.getChildNodes
    (0 until children.getLength).map(children.item).collect {
      case element: Element if element.getNodeType == Node.ELEMENT_NODE && element.getTagName == name => element
    }
  }
}
