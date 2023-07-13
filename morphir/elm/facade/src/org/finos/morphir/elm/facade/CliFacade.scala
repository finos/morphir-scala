package org.finos.morphir.elm.facade

trait CliFacade[F[_]] {

  def make(
      projectDir: String = ".",
      outputPath: String = "morphir.json",
      typesOnly: Boolean = false,
      fallbackCli: Boolean = false
  ): F[Option[String]]
}

object CliFacade {
  def apply[F[_]](implicit instance: CliFacade[F]): CliFacade[F] = instance
}
