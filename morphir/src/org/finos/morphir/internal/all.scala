package org.finos.morphir.internal

trait AllTypeLevelModules
    extends AccessControlledModule
    with DocumentedModule
    with TypeModule
    with TypeSpecModule
    with TypeDefModule
    with TypeInfoModule
    with TypeOfModule
    with TypeTransformerModule {}

trait AllMiscModules extends AccessControlledModule with DocumentedModule
