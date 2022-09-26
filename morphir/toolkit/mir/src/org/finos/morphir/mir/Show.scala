package org.finos
package morphir
package mir

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import org.finos.morphir.util.{ShowBuilder, unreachable}
import org.finos.morphir.util.ShowBuilder.InMemoryShowBuilder

import java.io.PrintWriter

object Show:
  def newBuilder: MirShowBuilder = new MirShowBuilder(new InMemoryShowBuilder)

  def debug[T](msg: String)(f: => T): T =
    val value = f
    println("$msg: " + value)
    value
  end debug

  def apply(v: Attr): String =
    val b = newBuilder; b.attr_(v); b.toString

  def apply(v: Attrs): String =
    val b = newBuilder; b.attrs_(v); b.toString

  def apply(v: Defn): String =
    val b = newBuilder; b.defn_(v); b.toString

  def apply(v: Global): String =
    val b = newBuilder; b.global_(v); b.toString
  def apply(v: Local): String =
    val b = newBuilder; b.local_(v); b.toString
  def apply(v: Spec): String =
    val b = newBuilder; b.spec_(v); b.toString

  def apply(v: Type): String =
    val b = newBuilder; b.type_(v); b.toString

  def dump(defns: Seq[Defn], fileName: String): Unit =
    val pw = new PrintWriter(fileName)
    try
      defns
        .filter(_ != null)
        .sortBy(_.name)
        .foreach { defn =>
          pw.write(defn.show)
          pw.write("\n")
        }
    finally
      pw.close()
  end dump

  final class MirShowBuilder(val builder: ShowBuilder) extends AnyVal:
    import builder._

    def attr_(attr: Attr): Unit = ()
    def attrs_(attrs: Attrs): Unit =
      if (attrs == Attrs.None) {
        ()
      } else {
        attrs_(attrs.toSeq)
      }

    def attrs_(attrs: Seq[Attr]): Unit =
      rep(attrs, sep = " ")(attr_)

    def defn_(defn: Defn): Unit = defn match
      case Defn.Module(attrs, name)    => ()
      case Defn.TypeAlias(attrs, name) => ()

    def defns_(defns: Seq[Defn]): Unit =
      rep(defns) { defn =>
        newline()
        defn_(defn)
      }

    def global_(fqn: Global): Unit = fqn match
      case Global.None =>
        unreachable
      case Global.Top(id) =>
        str(id)
      case Global.Member(module, spec) =>
        global_(module)
        str("/")
        spec_(spec)
    end global_

    def local_(local: Local): Unit =
      str("%")
      str(local.id)

    def spec_(spec: Spec): Unit = ()

    def type_(typ: Type): Unit = ()

    private def escapeQuotes(s: String): String =
      val chars   = s.toArray
      val out     = mutable.UnrolledBuffer.empty[Char]
      var i       = 0
      var escaped = false
      while (i < chars.length)
        val char = chars(i)
        char match
          case '"' =>
            if (!escaped) out += '\\'
            out += char
          case _ =>
            out += char
        escaped = char == '\\'
        i += 1
      end while
      new String(out.toArray)

    end escapeQuotes

    override def toString: String = builder.toString

  end MirShowBuilder
end Show
