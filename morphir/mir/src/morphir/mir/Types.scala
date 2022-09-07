package morphir.mir
import morphir.util.{unreachable, unsupported}

sealed abstract class Type:
  def show: String = morphir.mir.Show(this)
object Type
