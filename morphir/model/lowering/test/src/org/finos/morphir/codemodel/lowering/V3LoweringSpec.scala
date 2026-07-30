package org.finos.morphir.codemodel.lowering

import kyo.test.*
import kyo.Chunk
import org.finos.morphir.naming.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.ir.Type as v3

class V3LoweringSpec extends Test[Any]:

  "lowers a Unit type" in
    assert(V3Lowering.lowerType(v3.Type.Unit(())) == cm.Type.Unit(cm.TypeAttributes.empty))

  "lowers a Variable type, preserving the name" in {
    val name = Name.fromString("a")
    assert(
      V3Lowering.lowerType(v3.Type.Variable((), name)) ==
        cm.Type.Variable(cm.TypeAttributes.empty, name)
    )
  }

  "lowers a Reference with arguments recursively" in {
    val fq  = FQName.fromString("Morphir.SDK:Basics:maybe")
    val in  = v3.Type.Reference((), fq, List(v3.Type.Unit(())))
    val out = cm.Type.Reference(cm.TypeAttributes.empty, fq, Chunk(cm.Type.Unit(cm.TypeAttributes.empty)))
    assert(V3Lowering.lowerType(in) == out)
  }

end V3LoweringSpec
