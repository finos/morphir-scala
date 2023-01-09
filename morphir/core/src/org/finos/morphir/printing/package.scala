package org.finos.morphir

package object printing:

  // TODO: Use Castor to manager customizing this by having a Map from a izumi type tag to a function that works on that tag
  // type Key[A:Tag] => PPrintConfig[A]
  private[printing] val pprintCustom = pprint.copy()
  export pprintCustom.*
  def stringify[A: Stringify](value: A): String = summon[Stringify[A]].stringify(value)

  def stringifyLine[A: Stringify](value: A): String = summon[Stringify[A]].stringifyLine(value)
