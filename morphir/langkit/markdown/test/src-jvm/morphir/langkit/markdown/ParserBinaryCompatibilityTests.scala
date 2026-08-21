package morphir.langkit.markdown

import java.lang.reflect.Modifier
import kyo.test.*
import morphir.MorphirException

class ParserBinaryCompatibilityTests extends Test[Any]:

  "Parser JVM API" - {
    // The profile slice gave the shortest `parse` a `using MdProfile` clause, which the JVM spells as a second
    // parameter: `parse(String)` became `parse(String, MdProfile)`. The pin moves with it — pre-1.0, the surface is
    // free to change, and what this test defends is that it changes on purpose rather than by accident.
    "retains the shortest parse descriptor, now carrying the profile" in {
      val method = Parser.getClass.getMethods.find { candidate =>
        candidate.getName == "parse" &&
        candidate.getParameterTypes.toList == List(classOf[String], classOf[MdProfile])
      }.getOrElse(throw new AssertionError("missing parse(String, MdProfile)"))

      val result = method.invoke(Parser, "# Title", MdProfile.commonmark)
      assert(result != null)
    }

    "publishes ParseError as a MorphirException hierarchy" in {
      assert(Modifier.isAbstract(classOf[ParseError].getModifiers))
      assert(classOf[MorphirException].isAssignableFrom(classOf[ParseError]))
      assert(classOf[ParseError].isAssignableFrom(classOf[ParseError.Syntax]))
      assert(classOf[ParseError].isAssignableFrom(classOf[ParseError.Scan]))
    }
  }
