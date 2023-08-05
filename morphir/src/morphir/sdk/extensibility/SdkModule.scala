package morphir.sdk.extensibility
import org.finos.morphir.extensibility.MorphirModule
abstract class SdkModule private[sdk] (val packageName: String, val moduleName: String) extends MorphirModule {}
