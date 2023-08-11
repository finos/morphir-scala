package org.finos.morphir.internal

trait AllTypeLevelModules extends AccessControlledModule with DocumentedModule with TypeModule with typeSpec
    with TypeDefModule
    with TypeTransformerModule {}
