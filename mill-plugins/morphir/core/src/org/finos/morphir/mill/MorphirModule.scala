package org.finos.morphir.mill

import mill.*

trait MorphirModule extends Module {
  def moduleId: T[ModuleId] = Task {
    val resolvedModule    = os.Path(moduleDir.toNIO.toRealPath())
    val resolvedWorkspace = os.Path(mill.api.BuildCtx.workspaceRoot.toNIO.toRealPath())
    val value             = resolvedModule.relativeTo(resolvedWorkspace).segments.toSeq.mkString(".")
    ModuleId.parse(value).fold(throw _, identity)
  }

  /** Direct Morphir dependencies whose typed IR artifacts feed this module. */
  def morphirModuleDeps: Seq[MorphirModule] = Seq.empty

  final def morphirModuleDepsChecked: Seq[MorphirModule] = {
    recursiveMorphirModuleDeps
    morphirModuleDeps
  }

  private lazy val recursiveMorphirModuleDeps: Seq[MorphirModule] = {
    def collect(dependencies: Seq[MorphirModule], seen: Set[MorphirModule]): Seq[MorphirModule] =
      dependencies.flatMap { dependency =>
        if (seen.contains(dependency))
          throw new IllegalArgumentException(s"Cyclic Morphir module dependency involving ${dependency.moduleDir}")
        else dependency +: collect(dependency.morphirModuleDeps, seen + dependency)
      }

    collect(morphirModuleDeps, Set(this))
  }

  def dependencyArtifacts: Task[Seq[MorphirDependencyArtifact]] = Task.Anon {
    Task.traverse(morphirModuleDepsChecked) { dependency =>
      Task.Anon {
        MorphirDependencyArtifact.fromArtifact(dependency.morphirIR())
      }
    }()
  }

  def morphirIR: T[MorphirIrArtifact]
}
