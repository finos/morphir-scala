# Morphir 

<a href="https://finosfoundation.atlassian.net/wiki/display/FINOS/Active">
    <img src="https://cdn.jsdelivr.net/gh/finos/contrib-toolbox@master/images/badge-active.svg" />
</a>
<a href="https://github.com/finos/morphir-scala/actions">
    <img src="https://github.com/finos/morphir-scala/workflows/CI/badge.svg" />
</a>

<a href="https://finos-lf.slack.com/messages/morphir/">
    <img src="https://img.shields.io/badge/slack-@finos/morphir-green.svg?logo=slack" />
</a>
<a href="https://gitpod.io/#https://github.com/finos/morphir-scala">
    <img src="https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod" />
</a>

[![Scaladoc](https://img.shields.io/badge/Scaladoc-latest-blue)](https://javadoc.io/doc/org.finos.morphir/morphir-runtime_3/latest/index.html) [![Scaladex](https://img.shields.io/maven-central/v/org.finos.morphir/morphir-runtime_3?style=flat-square&label=Scaladex)]("https://index.scala-lang.org/finos/morphir-scala") [![Sonatype Releases](https://img.shields.io/nexus/r/https/oss.sonatype.org/org.finos.morphir/morphir_3.svg?label=Sonatype%20Release)](https://oss.sonatype.org/content/repositories/releases/org/finos/morphir/morphir_3/) [![Sonatype Snapshots](https://img.shields.io/nexus/s/https/oss.sonatype.org/org.finos.morphir/morphir_3.svg?label=Sonatype%20Snapshot)](https://oss.sonatype.org/content/repositories/snapshots/org/finos/morphir/morphir_3/)


Morphir is a library of tools that works to capture business logic as data.

For the first time, business logic can be shared, stored, translated and visualised, all with the reliability of standardisation ensured in the Morphir framework.

## morphir-scala

Provides Scala language bindings and JVM based tooling for Morphir.

## Installation

You can install the `morphir-cli` in the following ways:

**Using Coursier**

```
cs install --channel https://raw.githubusercontent.com/finos/morphir-scala/main/coursier-channel.json morphir-cli
```

Then run the CLI:

```
morphir-cli setup
```

We also offer an insiders channel that grants access to snapshot releases of the CLI.

```
cs install --channel https://raw.githubusercontent.com/finos/morphir-scala/main/coursier-channel.json morphir-insiders-cli
```

Then run the CLI:

```
morphir-cli setup
```

NOTE: The main channel above (non-insiders), also offers insiders builds under the name `morphir-insiders-cli`:

```
cs install --channel https://raw.githubusercontent.com/finos/morphir-scala/main/coursier-channel.json morphir-insiders-cli
```

Then run the CLI:

```
morphir-insiders-cli setup
```

---
## Development

### Gitpod

Click the button below to start a new development environment and start contributing right away:


[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/finos/morphir-scala)

### How to build and test

Morphir-jvm use [mill](https://com-lihaoyi.github.io/mill) as its build tool.

#### IntelliJ Setup for Linux

If you are using IntelliJ IDEA to edit morphir-jvm's Scala code, you can create the
IntelliJ project files via:

````bash
./mill mill.idea.GenIdea/idea
````

#### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```
./mill mill.bsp.BSP/install
```

You can also try the following to use the `mill-contrib-bloop` plugin to install and use the Bloop based BSP setup.

> NOTE: The recommended BSP based setup for mill is to run: `./mill mill.bsp.BSP/install` as mentioned above.





#### Run Tests

```bash
./mill __.test
```

or in watch mode:

```bash
./mill -w __.test
```

> NOTE: When running tests you will want to ensure you have generated the MorphirIR for elm sources if you are running tests for the runtime. A simple way to do this is to use the `build.sh` helper script an invoke the `elm-build` option. `./build.sh elm-build`

For your convience, you can run the morphir-runtime tests using the following command:


```bash
./build.sh test-runtime-jvm
```

#### Test Coverage

Before checking test coverage, ensure that all tests have been run using the following command:

```bash
./mill __.test
```

##### HTML Coverage Report

To generate a human-readable coverage report, run the following command:

```bash
./mill scoverage.htmlReportAll
```

To view the resulting coverage report, open `out/scoverage/htmlReportAll.dest/index.html`

##### XML Coverage Report

To generate an XML coverage report, run the following command:

```bash
./mill scoverage.xmlReportAll
```

The report will be generated at this location `out/scoverage/xmlReportAll.dest/scoverage.xml`

#### Formatting Scala Code

Code needs to be formatted according to `scalafmt` rules. Mill owns the entrypoint:

```bash
./mill format --kind scala
```

For a few known files (preferred during iteration):

```bash
./mill format --paths path/to/File.scala
```

or git-touched paths:

```bash
./mill format --changed
```

See [CONTRIBUTING.md — Formatting](./CONTRIBUTING.md#formatting) for the full command table (`--sources`, `--check`, etc.).

#### Formatting Elm Code

Elm sources under `examples/morphir-elm-projects` and `morphir-elm` are formatted with the repo-pinned elm-format binary via Mill (no global `npm install -g elm-format`):

```bash
./mill format --kind elm
```

or a single project / path:

```bash
./mill format --kind elm --paths examples/morphir-elm-projects/evaluator-tests/src
```

More detail for the evaluator-tests Elm tree:
[examples/morphir-elm-projects/evaluator-tests/ReadMe.md](./examples/morphir-elm-projects/evaluator-tests/ReadMe.md)

#### IntelliJ Setup for Windows

If you are using IntelliJ IDEA to edit morphir-scala's Scala code, you can create the
IntelliJ project files via or use the **BSP Setup** option (BSP is the recommended approach):

```
.\mill -i mill.scalalib.GenIdea/idea
```

#### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```
.\mill mill.bsp.BSP/install
```

You can also try the following to use the `mill-contrib-bloop` plugin to install and use the Bloop based BSP setup.

> NOTE: The recommended BSP based setup for mill is to run: `.\mill mill.bsp.BSP/install` as mentioned above.

```
.\mill -i --import ivy:com.lihaoyi::mill-contrib-bloop:  mill.contrib.bloop.Bloop/install
```

#### Run Tests

```
.\mill -i __.test
```

or in watch mode:

```
.\mill -i -w __.test
```

#### Elm Tests

Documentation for the elm tests are located at [examples/morphir-elm-projects/evaluator-tests/ReadMe.md](https://github.com/finos/morphir-scala/blob/main/examples/morphir-elm-projects/evaluator-tests/ReadMe.md)

#### Formatting Code

Code needs to be formatted according to `scalafmt` / elm-format rules. Prefer the Mill entrypoint:

```
.\mill -i format
```

Narrow selection during iteration:

```
.\mill -i format --changed
.\mill -i format --paths path\to\File.scala
```

See [CONTRIBUTING.md — Formatting](./CONTRIBUTING.md#formatting).

### Roadmap

Watch for updates.

### Contributing

1. Fork it (<https://github.com/finos/morphir-scala/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Read our [contribution guidelines](CONTRIBUTING.md) and [Community Code of Conduct](https://www.finos.org/code-of-conduct)
4. Commit your changes (`git commit -am 'Add some fooBar'`)
5. Push to the branch (`git push origin feature/fooBar`)
6. Create a new Pull Request

_NOTE:_ Commits and pull requests to FINOS repositories will only be accepted from those contributors with an active, executed Individual Contributor License Agreement (ICLA) with FINOS OR who are covered under an existing and active Corporate Contribution License Agreement (CCLA) executed with FINOS. Commits from individuals not covered under an ICLA or CCLA will be flagged and blocked by the FINOS Clabot tool (or [EasyCLA](https://github.com/finos/community/blob/master/governance/Software-Projects/EasyCLA.md)). Please note that some CCLAs require individuals/employees to be explicitly named on the CCLA.

*Need an ICLA? Unsure if you are covered under an existing CCLA? Email [help@finos.org](mailto:help@finos.org)*

### License

Copyright 2022 FINOS

Distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).

SPDX-License-Identifier: [Apache-2.0](https://spdx.org/licenses/Apache-2.0)

[Link-Slack]: https://finos-lf.slack.com/messages/morphir/
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/org/finos/morphir/morphir-runtime_3/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/org/finos/morphir/morphir-runtime_3/ "Sonatype Snapshots"

[Badge-CI]: https://github.com/finos/morphir-scala/workflows/CI/badge.svg
[Badge-Slack]: https://img.shields.io/badge/slack-@finos/morphir-green.svg?logo=slack
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/org.finos.morphir/morphir-runtime_3.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/org.finos.morphir/morphir-runtime_3.svg "Sonatype Snapshots"
[Badge-Stage]: https://cdn.jsdelivr.net/gh/finos/contrib-toolbox@master/images/badge-incubating.svg
[Badge-Stage-Page]: https://finosfoundation.atlassian.net/wiki/display/FINOS/Incubating
