# Contributing to the `morphir` module

This file covers dev notes specific to the `morphir` module (the root Morphir library — `morphir.jvm`/`morphir.js`) that don't belong in the root [CONTRIBUTING.md](../CONTRIBUTING.md) (project governance/process) or [AGENTS.md](../AGENTS.md) (general AI agent guidelines). See those first; this file only covers what's local to this module.

## Known issue: `morphir` scaladoc generation is disabled

Both `morphir.jvm` and `morphir.js` mix in the `EmptyDocJar` trait from the root [`build.mill`](../build.mill), via
their `extends:` entries in [`package.mill.yaml`](./package.mill.yaml). It overrides `docJar` to produce an empty jar
instead of running scaladoc:

```scala
trait EmptyDocJar extends JavaModule {
  override def docJar: T[PathRef] = Task {
    PathRef(mill.util.Jvm.createJar(Task.dest / "out.jar", Seq.empty))
  }
}
```

The crash follows the *sources*, not the platform, and both variants compile the same ones — so `morphir.js` needed
the same workaround as `morphir.jvm`. It went unnoticed for a while because it only appears on a cold cache, and
because of the misleading symptom described below.

### Why

Generating scaladoc for these modules reliably (though nondeterministically — it can occasionally succeed) crashes
with:

```
java.lang.NullPointerException: Cannot invoke "scala.collection.immutable.List.$plus$plus(scala.collection.IterableOnce)"
because the return value of "dotty.tools.scaladoc.translators.SignatureBuilder.content()" is null
```

This is a known upstream bug in Scala 3's scaladoc tool (`dotty`), not anything in our code:
[scala/scala3#24183](https://github.com/scala/scala3/issues/24183). It's specific to running scaladoc on **JDK 25** and is flaky/nondeterministic by nature (other projects report it "always" happening, or disappearing after unrelated source tweaks — see the issue thread). It's fixed by [scala/scala3#25779](https://github.com/scala/scala3/pull/25779), but that fix landed in the **3.9.0** milestone, after our pinned `3.8.4` was already feature-frozen. There is no 3.8.x or 3.3.x LTS backport.

We ruled out:
- Anything specific to *our* code being the trigger — the crash rate didn't change whether `morphir.jvm` was built alone or alongside its `extensibility` module dependency, and editing/removing doc comments in dependency modules made no difference across repeated trials.
- A newer Mill build fixing it — reproduces identically on both stable Mill `1.2.0-RC1` and the latest published unstable Mill build at the time of investigation.

### How it reports itself

Worth knowing, because the message you see is not the crash. Once the scaladoc worker dies, Mill's retry reports the
missing output directory:

```
'.../out/morphir/js/scalaDocGenerated.dest/javadoc' does not exist or is not a directory or .jar file
  scaladoc -help  gives more information
```

The underlying `NullPointerException` appears only under `./mill --debug <target>`. Reach for `--debug` before
concluding this is a different bug.

Because it needs a cold cache, a warm `out/` hides it — the failing task is `publishArtifacts`, so it surfaces as
`mise run test:js` or `test:jvm` failing on CI while passing locally. To reproduce, clear the caches first:

```bash
./mill clean morphir.js
./mill morphir.js.publishArtifacts
```

### When to remove this workaround

Once `ScalaVersions.scala3` in [`deps.scala`](../mill-build/src/millbuild/deps.scala) is bumped to Scala `3.9.0` or
later (or a `3.8.x`/`3.3.x` backport ships with the fix), drop `build.EmptyDocJar` from both the `jvm` and `js`
`extends:` lists in `package.mill.yaml`, and confirm `./mill morphir.jvm.docJar` and `./mill morphir.js.docJar`
succeed without it across several repeated runs — from a cold cache each time — before relying on it in CI. If no
other module needs the trait by then, remove it from `build.mill` too.
