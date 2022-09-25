package org.finos.morphir.formats

trait HasContent[+A]:
  def content: A
