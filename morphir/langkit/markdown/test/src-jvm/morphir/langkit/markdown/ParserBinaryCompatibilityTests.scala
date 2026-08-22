package morphir.langkit.markdown

import java.lang.reflect.Modifier
import kyo.test.*
import morphir.MorphirException

/**
 * What the JVM sees of the parse surface, pinned so it changes on purpose rather than by accident.
 *
 * The pin moved with the surface. `Parser` is now `private[markdown]` machinery under
 * `morphir.langkit.markdown.internal`, reached from outside only through [[MD.parser]], so it is the facade's forwarder
 * that is worth pinning: that is the descriptor a published consumer links against. Pre-1.0 the surface is free to
 * change, and this test defends deliberateness, not stability.
 */
class ParserBinaryCompatibilityTests extends Test[Any]:

  "MD.parser JVM API" - {
    // The profile slice gave the shortest `parse` a `using MdProfile` clause, which the JVM spells as a second
    // parameter: `parse(String)` became `parse(String, MdProfile)`. The facade forwards it at the same descriptor.
    "retains the shortest parse descriptor, now carrying the profile" in {
      val method = MD.parser.getClass.getMethods.find { candidate =>
        candidate.getName == "parse" &&
        candidate.getParameterTypes.toList == List(classOf[String], classOf[MdProfile])
      }.getOrElse(throw new AssertionError("missing parse(String, MdProfile)"))

      val result = method.invoke(MD.parser, "# Title", MdProfile.commonmark)
      assert(result != null)
    }

    "publishes MdParseError as a MorphirException hierarchy" in {
      assert(Modifier.isAbstract(classOf[MdParseError].getModifiers))
      assert(classOf[MorphirException].isAssignableFrom(classOf[MdParseError]))
      assert(classOf[MdParseError].isAssignableFrom(classOf[MdParseError.Syntax]))
      assert(classOf[MdParseError].isAssignableFrom(classOf[MdParseError.Scan]))
    }
  }
