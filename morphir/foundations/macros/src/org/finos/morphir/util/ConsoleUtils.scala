/*
 * Copyright 2023 Morphir Contributors
 * Copyright 2018-2023 John A. De Goes and the ZIO Contributors
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package org.finos.morphir.foundations

/**
 * Ported from ZIO Prelude
 * https://github.com/zio/zio-prelude/blob/754ced18f21573d1bc128715ceb24899d1e67353/macros/shared/src/main/scala/ConsoleUtils.scala
 */
private[foundations] object ConsoleUtils {
  def underlined(s: String): String =
    Console.UNDERLINED + s + Console.RESET

  def green(s: String): String =
    Console.GREEN + s + Console.RESET

  def yellow(s: String): String =
    Console.YELLOW + s + Console.RESET

  def red(s: String): String =
    Console.RED + s + Console.RESET

  def blue(s: String): String =
    Console.BLUE + s + Console.RESET

  def magenta(s: String): String =
    Console.MAGENTA + s + Console.RESET

  def cyan(s: String): String =
    Console.CYAN + s + Console.RESET

  def dim(s: String): String =
    "\u001b[2m" + s + Console.RESET

  def bold(s: String): String =
    Console.BOLD + s + Console.RESET

}
