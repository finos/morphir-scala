package org.finos.morphir
import org.finos.morphir.naming.*

trait MorphirTag[A] {
  def nodeID: NodeID
}

object MorphirTag {
  def apply[A](implicit ev: MorphirTag[A]): MorphirTag[A] = ev

  trait Has[A] {
    def getTag: MorphirTag[A]
  }

  trait Companion[A] extends MorphirTag[A] with Has[A] { self =>
    implicit val tagInstance: MorphirTag[A]  = self
    final override def getTag: MorphirTag[A] = self

    // object hint {
    //   def unapply(h: Hints): Option[A] = h.get[A]
    // }
  }
}
