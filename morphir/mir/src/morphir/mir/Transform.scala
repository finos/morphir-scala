package morphir.mir

trait Transform:
  def onDefns(assembly: Seq[Defn]): Seq[Defn] =
    assembly
