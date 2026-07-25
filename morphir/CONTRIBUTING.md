# Contributing to the `morphir` module

This file covers dev notes specific to the `morphir` module (the root Morphir library — `morphir.jvm`/`morphir.js`) that don't belong in the root [CONTRIBUTING.md](../CONTRIBUTING.md) (project governance/process) or [AGENTS.md](../AGENTS.md) (general AI agent guidelines). See those first; this file only covers what's local to this module.

## Known issue: `morphir.jvm` scaladoc generation is disabled

`morphir.jvm`'s `docJar` is overridden in [`package.mill`](./package.mill) to produce an empty jar instead of running scaladoc:

```scala
object jvm extends Shared with MorphirJVMModule {
  override def docJar: T[PathRef] = Task {
    PathRef(mill.util.Jvm.createJar(Task.dest / "out.jar", Seq.empty))
  }
}
```

### Why

Generating scaladoc for `morphir.jvm` reliably (though nondeterministically — it can occasionally succeed) crashes with:

```
java.lang.NullPointerException: Cannot invoke "scala.collection.immutable.List.$plus$plus(scala.collection.IterableOnce)"
because the return value of "dotty.tools.scaladoc.translators.SignatureBuilder.content()" is null
```

This is a known upstream bug in Scala 3's scaladoc tool (`dotty`), not anything in our code:
[scala/scala3#24183](https://github.com/scala/scala3/issues/24183). It's specific to running scaladoc on **JDK 25** and is flaky/nondeterministic by nature (other projects report it "always" happening, or disappearing after unrelated source tweaks — see the issue thread). It's fixed by [scala/scala3#25779](https://github.com/scala/scala3/pull/25779), but that fix landed in the **3.9.0** milestone, after our pinned `3.8.4` was already feature-frozen. There is no 3.8.x or 3.3.x LTS backport.

We ruled out:
- Anything specific to *our* code being the trigger — the crash rate didn't change whether `morphir.jvm` was built alone or alongside its `extensibility` module dependency, and editing/removing doc comments in dependency modules made no difference across repeated trials.
- A newer Mill build fixing it — reproduces identically on both stable Mill `1.2.0-RC1` and the latest published unstable Mill build at the time of investigation.

### When to remove this workaround

Once `morphirScalaVersion` in `build.mill` is bumped to Scala `3.9.0` or later (or a `3.8.x`/`3.3.x` backport ships with the fix), remove the `docJar` override in `package.mill` and confirm `./mill morphir.jvm.docJar` (and `./mill morphir.extensibility.jvm.publishArtifacts + morphir.jvm.publishArtifacts` for a fuller repro) succeed without it across several repeated runs before relying on it in CI.
