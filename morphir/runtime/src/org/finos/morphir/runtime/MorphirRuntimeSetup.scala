package org.finos.morphir.runtime

import org.finos.morphir.AllIRModules
import org.finos.morphir.runtime.extensions._
case class MorphirRuntimeSetup[+TA, +VA](ir: AllIRModules)(extensions: List[RuntimeExtension])
