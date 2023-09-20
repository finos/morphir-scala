package org.finos.morphir.internal

trait AllTypeLevelModules
    extends AccessControlledModule
    with TypeSpecModule
    with TypeDefModule
    with TypeInfoModule
    with TypeOfModule {}

trait AllMiscModules extends AccessControlledModule
