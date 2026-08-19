package morphir.langkit.markdown

import java.lang.reflect.Modifier
import kyo.test.*
import morphir.MorphirException

class ParserBinaryCompatibilityTests extends Test[Any]:

  "Parser JVM API" - {
    "retains the one-argument parse descriptor" in {
      val method = Parser.getClass.getMethods.find { candidate =>
        candidate.getName == "parse" &&
        candidate.getParameterTypes.toList == List(classOf[String])
      }.getOrElse(throw new AssertionError("missing parse(String)"))

      val result = method.invoke(Parser, "# Title")
      assert(result != null)
    }

    "publishes ParseError as a MorphirException hierarchy" in {
      assert(Modifier.isAbstract(classOf[ParseError].getModifiers))
      assert(classOf[MorphirException].isAssignableFrom(classOf[ParseError]))
      assert(classOf[ParseError].isAssignableFrom(classOf[ParseError.Syntax]))
      assert(classOf[ParseError].isAssignableFrom(classOf[ParseError.Scan]))
    }
  }
