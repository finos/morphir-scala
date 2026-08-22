# Decision Records

Architectural decisions for morphir-scala, recorded as prose and **superseded rather than edited**. A record captures
what was decided, which alternatives were rejected, and under what condition it should be revisited — so the
reasoning available at the time survives even after the conclusion changes.

This is the third register in the knowledge base. An [Intent](../../../intent/index.md) is future-tense and answers
*should we do this*; a Capability is present-tense and answers *what does the system do*; a Decision Record is
past-tense and answers *why is it shaped this way*. See
[0004](/decisions/0004-decision-records-are-a-third-register.md) for that reasoning.

```bash
.claude/skills/kb/kb decision list --in-force
.claude/skills/kb/kb decision show 0005
```

## Knowledge base and process

* [Released intent stays; capabilities are separate documents](/decisions/0001-released-intent-stays-capabilities-are-separate.md) - Intent records are never moved on release; a Released intent must link to a separate present-tense Capability document.
* [Intent tooling lives in the kb skill](/decisions/0002-intent-tooling-lives-in-the-kb-skill.md) - Intent management is implemented as `kb intent …` subcommands inside the kb skill, not as a separate skill with its own code.
* [Two identifier schemes for intent, deliberately](/decisions/0003-two-identifier-schemes-for-intent.md) - Documents are addressed as `bundle-label:/path.md` and published software by Package URL; neither scheme is legacy.
* [Decision Records are a third register in the knowledge base](/decisions/0004-decision-records-are-a-third-register.md) - Architectural decisions are recorded as `type: Decision Record` concepts that are superseded rather than edited, alongside Intent and Capability.

## Runtime and code model

* [Bridge nothing between ZIO and Kyo](/decisions/0005-bridge-nothing-between-zio-and-kyo.md) - New code is written against Kyo, existing ZIO code is left untouched, and no ZIO-to-Kyo adapter is permitted at the boundary.
* [Runtime values are one serializable Val built on kyo-schema](/decisions/0006-runtime-values-are-val-on-kyo-schema.md) - The new runtime has a single value type, datamodel.Val, whose structured case is kyo-schema's Structure.Value and whose closures serialize.
* [The runtime reuses the code model's type language and targets v4](/decisions/0007-runtime-reuses-the-code-model-type-language.md) - Runtime types are codemodel.Type rather than a second type language, and the new runtime targets the v4 code model with a one-way lowering from v3.
* [morphir/model and morphir/naming are dependency-constrained modules](/decisions/0008-model-and-naming-are-dependency-constrained-modules.md) - The code model and the naming vocabulary were extracted into modules whose dependency closures are constrained by construction, not by convention.
* [Expressions are Expr, values are Val — diverging from Morphir's Elm-inherited vocabulary](/decisions/0009-expressions-are-expr-values-are-val.md) - The code model's expression type is renamed from Value to Expr, so that the word value is free for what an expression evaluates to.
* [The old runtime becomes runtime.classic; its package rename is deferred](/decisions/0010-the-old-runtime-becomes-runtime-classic.md) - The existing ZIO runtime moved to morphir/runtime/classic intact, so the new runtime can take the good module path without a flag-day cutover.
* [Runtime closures retain parameter patterns](/decisions/0011-runtime-closures-retain-parameter-patterns.md) - Val.Closure stores each remaining parameter as a code-model Pattern, preserving destructuring lambdas in the serializable runtime value.

## Build and tooling

* [Keep compiling Mill Morphir plugins into the metabuild](/decisions/0012-keep-source-metabuild-for-mill-morphir-plugins.md) - Normal builds continue to compile mill-plugins/morphir sources into the metabuild; pinned Central artifacts are deferred until bootstrap experience is measured.
* [Trunk-based development on main; the develop branch is retired](/decisions/0014-trunk-based-development-on-main.md) - Pull requests target main and merge into it. The develop integration branch and its promotion ritual are removed, because the second branch cost more than it returned.

## Language toolkits

* [Profile-dependent syntax branches at the earliest stage whose tree can record it](/decisions/0015-profile-branches-at-the-earliest-capable-stage.md) - A parse profile changes behaviour at the earliest pipeline stage whose output tree can record the decision, and no later, so the concrete and abstract syntax trees never disagree.

## Published libraries

* [Published library families are kit, connector, appkit, langkit, and knowledge](/decisions/0013-published-library-families.md) - Kit wraps Scala libraries, connector wraps external systems, appkit hosts Morphir in an application, and knowledge holds OKF; kit and connector both compile for JVM, JS, and Native.
