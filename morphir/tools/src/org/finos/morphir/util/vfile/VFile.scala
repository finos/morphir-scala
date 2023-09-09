package org.finos.morphir.util.vfile

import org.finos.morphir.util.attribs.{Attribute, Attributes}

import java.nio.file.Path
import org.typelevel.paiges._

final case class VFile(path: VPath, contents: VFileContents, properties: Attributes, data: Attributes) { self =>
  import Attribute.Binding
  def ++=(bindings: Seq[Binding[_]]): VFile = copy(properties = properties ++= bindings)
//  def accept[A](visitor:ExternalVisitor[A]):A = self match {
//    case FileReference(path) => visitor.fileReference(path)
//    case SourceFile(path, document) => visitor.sourceFile(path, document)
//    case Directory(path, children) => visitor.directory(path, children)
//  }
//  def accept[A](visitor: InternalVisitor[A]): A = acceptInternal(visitor).result
//  private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]): TailCalls.TailRec[A]
}

object VFile extends VFilePlatformSpecific {
  def directory(path: VPath, children: VFile*): VFile =
    VFile(path, VFileContents.VFiles(children.toVector), Attributes.empty, Attributes.empty)
  def fileRef(path: VPath): VFile =
    VFile(path, contents = VFileContents.Uninitialized, properties = Attributes.empty, Attributes.empty)
  def mixed(path: VPath, contents: VFileContents*): VFile =
    VFile(path, VFileContents.Mixed(contents.toVector), Attributes.empty, Attributes.empty)
  def sourceFile(path: VPath, document: Doc): VFile =
    VFile(path, VFileContents.TextDocument(document), Attributes.empty, Attributes.empty)
  def sourceFile(path: VPath, text: String): VFile =
    VFile(path, VFileContents.Text(text), Attributes.empty, Attributes.empty)

  trait InternalVisitor[A] {
    def fileReference(path: VPath): A
    def sourceFile(path: VPath, document: Doc): A
    def directory(path: VPath, children: Vector[A]): A
  }

  trait ExternalVisitor[+A] {
    def fileReference(path: VPath): A
    def sourceFile(path: VPath, document: Doc): A
    def directory(path: VPath, children: Vector[VFile]): A
  }

}
