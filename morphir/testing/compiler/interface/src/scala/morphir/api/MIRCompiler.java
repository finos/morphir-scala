package scala.morphir.api;
import java.nio.file.Path;
/**
 * Morphir compiler api.
 * 
 * This API is used for testing the Morphir Scala compiler (the component that
 * compiles Scala trees to MIR, the pickled Morphir compiler).
 */
public interface MIRCompiler {
    /**
	 * Compiles the source code given and returns all the files produced during
	 * compilation.
	 *
	 * @param source The source code to compile.
	 * @return All the files produced during compilation (classfiles, tasty, mir, etc.)
	 */
    public Path[] compile(String source);

    /**
     * Compiles all the source files in `base` and returns all the files
     * produced during compilation.
     *
     * @param base The base directory containing the source files.
     * @return All the files produced during compilation (classfiles, tasty, mir, etc.)
     */
    public Path[] compile(Path base);
}
