package millbuild

/** Test selectors and deterministic process-shard selection for Scala Native CI. */
object NativeTestSelectors {
  val compileSelector: String = "morphir.__.native.__.compile"
  val publishSelector: String = "morphir.__.native.publishArtifacts"
  val testSelector: String    = "morphir.__.native.__.test"

  val prepareSelectors: Seq[String] = Seq(compileSelector, publishSelector)

  /**
   * Selects one exhaustive, disjoint shard after sorting and deduplicating Mill's resolved targets.
   *
   * The caller runs each shard in a separate daemonless Mill invocation. That process boundary is what releases the
   * Scala Native linker worker and its retained heap between shards; partitioning inside one evaluation would not.
   */
  def selectShard(
      resolved: Seq[String],
      shard: Int,
      shards: Int,
      what: String
  ): Either[String, Seq[String]] =
    if shards <= 0 then Left(s"$what: shards must be greater than zero")
    else if shard < 0 || shard >= shards then
      Left(s"$what: shard $shard is outside the valid range 0..${shards - 1}")
    else {
      val selected = resolved.distinct.sorted.zipWithIndex.collect {
        case (target, index) if index % shards == shard => target
      }
      if selected.isEmpty then Left(s"$what: no targets remain for shard $shard of $shards")
      else Right(selected)
    }
}
