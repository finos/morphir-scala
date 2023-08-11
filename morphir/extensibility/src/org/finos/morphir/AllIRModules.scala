package org.finos.morphir

import org.finos.morphir.internal.{AccessControlledModule, DocumentedModule, TypeDefModule, TypeModule}

trait AllIRModules extends AccessControlledModule with DocumentedModule with TypeModule with TypeSpecModule
    with TypeDefModule
    with TypeFolderModule {}
