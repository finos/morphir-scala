package morphir.langkit.markdown

import kyo.test.*

class ParserBinaryCompatibilityTests extends Test[Any]:

  "Parser JVM API" - {
    "retains the one-argument parse descriptor" in {
      val method = Parser.getClass.getMethods.find { candidate =>
        candidate.getName == "parse" &&
        candidate.getParameterTypes.toList == List(classOf[String])
      }

      assert(method.nonEmpty)
    }
  }
