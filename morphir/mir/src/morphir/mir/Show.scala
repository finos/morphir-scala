package morphir.mir

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import morphir.util.{ShowBuilder, unreachable}
import morphir.util.ShowBuilder.InMemoryShowBuilder

object Show:
  def newBuilder: MirShowBuilder = new MirShowBuilder(new InMemoryShowBuilder)

  def debug[T](msg: String)(f: => T): T = {
    val value = f
    println("$msg: " + value)
    value
  }

  def apply(v:Global):String = 
    val b = newBuilder; b.fqn_(v); b.toString
  def apply(v:Local):String = 
    val b = newBuilder; b.local_(v); b.toString
  def apply(v:Spec):String = 
    val b = newBuilder; b.spec_(v); b.toString

  final class MirShowBuilder(val builder:ShowBuilder) extends AnyVal:
    import builder._

    def fqn_(fqn:Global):Unit = fqn match
      case Global.None => 
        unreachable
      case Global.Top(id) => 
        str(id)
      case Global.Member(module, spec) => 
        fqn_(module)
        str("/")
        spec_(spec)

    def local_(local:Local):Unit =
      str("%")
      str(local.id)

    def spec_(spec:Spec):Unit = ()
    
    private def escapeQuotes(s: String): String = 
      val chars = s.toArray
      val out = mutable.UnrolledBuffer.empty[Char]
      var i = 0
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

    override def toString:String = builder.toString
  end MirShowBuilder
end Show
    

