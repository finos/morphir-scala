package org.finos.morphir.util.vfile

import org.finos.morphir.util.props.{Property, PropertyBag}

import java.nio.file.Path
import org.typelevel.paiges._

final case class VFile(path: VFilePath, contents: VFileContents, properties: PropertyBag, data: PropertyBag) { self =>
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

object VFile extends VFilePlatformSpecific {
  def directory(path: VFilePath, children: VFile*): VFile =
    VFile(path, VFileContents.VFiles(children.toVector), PropertyBag.empty, PropertyBag.empty)
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
