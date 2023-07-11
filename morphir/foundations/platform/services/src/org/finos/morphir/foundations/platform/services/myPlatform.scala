package org.finos.morphir.foundations.platform.services
import org.finos.morphir.foundations.platform.services.internal.PathPlatformSpecific

object myPlatform {  
  object fs {}
  object path extends PathPlatformSpecific {}    
  object process {}
}
