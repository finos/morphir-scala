package org.finos.morphir.runtime

import org.finos.morphir.internal.AllTypeLevelModules
import org.finos.morphir.runtime.extensions._
case class MorphirRuntimeSetup[+TA, +VA](ir: AllTypeLevelModules)(extensions: List[RuntimeExtension])
