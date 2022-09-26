package org.finos
package morphir
package mir

import java.io.OutputStream
import java.nio._

package object serialization:
  def serializeText(defns: Seq[Defn], buffer: ByteBuffer): Unit =
    val builder = Show.newBuilder
    builder.defns_(defns)
    buffer.put(builder.toString.getBytes())

  def serializeBinary(defns: Seq[Defn], out: OutputStream): Unit =
    new BinarySerializer().serialize(defns, out)
