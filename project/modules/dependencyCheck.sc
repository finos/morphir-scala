import $ivy.`org.owasp:dependency-check-core:7.1.1`
import mill._
import mill.define.Command
import mill.define.Discover
import mill.define.ExternalModule
import mill.eval.Evaluator
import mill.main.EvaluatorScopt
import mill.modules.Jvm
import mill.scalalib.Dep
import mill.scalalib.JavaModule
import mill.scalalib.Lib
import mill.scalalib.internal.ModuleUtils
import org.owasp.dependencycheck.Engine
import org.owasp.dependencycheck.agent.DependencyCheckScanAgent
import org.owasp.dependencycheck.data.nexus.MavenArtifact
import org.owasp.dependencycheck.dependency.naming.{GenericIdentifier, Identifier, PurlIdentifier}
import org.owasp.dependencycheck.dependency.{Confidence, Dependency, EvidenceType}
import org.owasp.dependencycheck.exception.ExceptionCollection
import org.owasp.dependencycheck.utils.{Settings, SeverityUtil}
import org.owasp.dependencycheck.utils.Settings.KEYS._

//import io.kipp.mill.github.dependency.graph.domain.Manifest

trait DependencyCheckModule extends JavaModule { self =>
  def dependencyCheckAnalyze =
    T {
      val cp = self.compileClasspath()
      cp.map(p => p.path)
    }

  private def initializeSettings(): Settings = {
    val settings = new Settings()
    settings
  }
}
