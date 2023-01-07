package org.finos.morphir.toolkit.vfile

import org.finos.morphir.toolkit.props.{Property, PropertyBag}
import org.typelevel.paiges.*

import scala.util.control.TailCalls
import java.nio.file.Path

final case class VFile(path: VFilePath, contents: VFileContents, properties: PropertyBag, data: PropertyBag) { self =>
  import VFile._
  import Property.Binding
  def ++=(bindings: Seq[Binding[_]]): VFile = copy(properties = properties ++= bindings)
//  def accept[A](visitor:ExternalVisitor[A]):A = self match {
//    case FileReference(path) => visitor.fileReference(path)
//    case SourceFile(path, document) => visitor.sourceFile(path, document)
//    case Directory(path, children) => visitor.directory(path, children)
//  }
//  def accept[A](visitor: InternalVisitor[A]): A = acceptInternal(visitor).result
//  private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]): TailCalls.TailRec[A]
}

object VFile {
  def directory(path: VFilePath, children: VFile*): VFile =
    VFile(path, VFileContents.VFiles(children.toVector), PropertyBag.empty, PropertyBag.empty)
  def fileRef(path: Path): VFile = fileRef(VFilePath(path))
  def fileRef(path: VFilePath): VFile =
    VFile(path, contents = VFileContents.Uninitialized, properties = PropertyBag.empty, PropertyBag.empty)
  def mixed(path: VFilePath, contents: VFileContents*): VFile =
    VFile(path, VFileContents.Mixed(contents.toVector), PropertyBag.empty, PropertyBag.empty)
  def sourceFile(path: VFilePath, document: Doc): VFile =
    VFile(path, VFileContents.TextDocument(document), PropertyBag.empty, PropertyBag.empty)
  def sourceFile(path: VFilePath, text: String): VFile =
    VFile(path, VFileContents.Text(text), PropertyBag.empty, PropertyBag.empty)

  trait InternalVisitor[A] {
    def fileReference(path: VFilePath): A
    def sourceFile(path: VFilePath, document: Doc): A
    def directory(path: VFilePath, children: Vector[A]): A
  }

  trait ExternalVisitor[+A] {
    def fileReference(path: VFilePath): A
    def sourceFile(path: VFilePath, document: Doc): A
    def directory(path: VFilePath, children: Vector[VFile]): A
  }

}
