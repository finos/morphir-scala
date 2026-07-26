package morphir.langkit.itest

import org.junit.platform.suite.api.*

@Suite
@IncludeEngines(Array("cucumber"))
@SelectClasspathResource("features")
class CucumberSuite
