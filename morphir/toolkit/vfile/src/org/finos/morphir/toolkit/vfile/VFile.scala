package org.finos.morphir.toolkit.vfile

import org.typelevel.paiges.*
import zio.prelude._

import scala.util.control.TailCalls
import java.nio.file.Path

final case class VFile(path:VFilePath, contents:VFileContents) { self =>
  import VFile._
//  def accept[A](visitor:ExternalVisitor[A]):A = self match {
//    case FileReference(path) => visitor.fileReference(path)
//    case SourceFile(path, document) => visitor.sourceFile(path, document)
//    case Directory(path, children) => visitor.directory(path, children)
//  }
//  def accept[A](visitor: InternalVisitor[A]): A = acceptInternal(visitor).result
//  private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]): TailCalls.TailRec[A]
}

object VFile {
  def directory(path:VFilePath, children:VFile*):VFile = VFile(path, VFileContents.VFiles(children.toVector))
  def fileRef(path:Path):VFile = fileRef(VFilePath(path))
  def fileRef(path:VFilePath):VFile =  VFile(path, contents = VFileContents.Uninitialized)
  def mixed(path:VFilePath, contents:VFileContents*):VFile = VFile(path, VFileContents.Mixed(contents.toVector))
  def sourceFile(path:VFilePath, document:Doc):VFile = VFile(path, VFileContents.TextDocument(document))
  def sourceFile(path:VFilePath, text:String):VFile = VFile(path, VFileContents.Text(text))


//  final case class FileReference(path:VFilePath) extends VFile { self =>
//    override private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]):TailCalls.TailRec[A] = TailCalls.done(visitor.fileReference(path))
//  }
//  final case class SourceFile(path:VFilePath, document:Doc) extends VFile { self =>
//    override private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]):TailCalls.TailRec[A] = TailCalls.done(visitor.sourceFile(path, document))
//  }
//  final case class Directory(path:VFilePath, children:Vector[VFile]) extends VFile { self =>
//    override private[vfile] def acceptInternal[A](visitor: InternalVisitor[A]):TailCalls.TailRec[A] =
//      children.foldLeft(TailCalls.done(Vector.empty[A])){
//        case (acc, vf) =>
//          acc.flatMap { current =>
//            TailCalls.tailcall(vf.acceptInternal(visitor)).map(current :+ _)
//          }
//      }.map{ a => visitor.directory(path, a)}
//  }

  trait InternalVisitor[A] {
    def fileReference(path:VFilePath):A
    def sourceFile(path:VFilePath, document:Doc):A
    def directory(path:VFilePath, children:Vector[A]):A
  }

  trait ExternalVisitor[+A] {
    def fileReference(path:VFilePath):A
    def sourceFile(path:VFilePath, document:Doc):A
    def directory(path:VFilePath, children:Vector[VFile]):A
  }

  final type Repr[+A] = Repr.Type[(VFilePath, A)]
  object Repr extends NewtypeF
}


