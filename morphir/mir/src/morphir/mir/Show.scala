package morphir.mir
import morphir.util.{ShowBuilder, unreachable}

object Show:
  def newBuilder: MirShowBuilder = new MirShowBuilder(new InMemoryShowBuilder)
  def debug[T](msg: String)(f: => T): T = {
    val value = f
    println("$msg: " + value)
    value
  }

  def apply(v:Fqn):String = ???

  final class MirShowBuilder(val builder:ShowBuilder) extends AnyVal:
    import builder._
    def fqn_(fqn:Fqn):Unit = fqn match
      case Fqn.None => 
        unreachable
    
    override def toString:String = builder.toString
  end MirShowBuilder
end Show
    

