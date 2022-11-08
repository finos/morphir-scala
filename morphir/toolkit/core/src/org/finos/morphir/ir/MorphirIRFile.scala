package org.finos.morphir
package ir

import org.finos.morphir.ir.Distribution.Distribution

final case class MorphirIRFile(version: MorphirIRVersion, distribution: Distribution)
