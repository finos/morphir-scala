package published

object Main {
  def main(args: Array[String]): Unit =
    println(s"published-consumer:${GeneratedMorphirModel.irSha256}:${GeneratedMorphirModel.hasUnpublishedReference}")
}
