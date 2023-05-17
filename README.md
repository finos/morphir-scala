| Project Stage | CI | Release | Snapshot |
| --- | --- | --- | --- | 
| [![Project stage][Badge-Stage]][Badge-Stage-Page] | ![CI][Badge-CI] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots] |

| Slack | Gitpod |
| --- | --- |
| [![Badge-Slack]][Link-Slack] | [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/finos/morphir-scala) |

# Morphir

Morphir is a library of tools that works to capture business logic as data.

For the first time, business logic can be shared, stored, translated and visualised, all with the reliability of standardisation ensured in the Morphir framework.

## morphir-scala

Provides Scala language bindings and JVM based tooling for Morphir.

## Gitpod

Click the button below to start a new development environment and start contributing right away:


[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/finos/morphir-scala)

## How to build and test

Morphir-jvm use [mill](https://com-lihaoyi.github.io/mill) as its build tool.

### IntelliJ Setup for Linux

If you are using IntelliJ IDEA to edit morphir-jvm's Scala code, you can create the
IntelliJ project files via:

```bash
./mill mill.scalalib.GenIdea/idea
```

### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```
./mill mill.bsp.BSP/install
```

You can also try the following to use the `mill-contrib-bloop` plugin to install and use the Bloop based BSP setup.

> NOTE: The recommended BSP based setup for mill is to run: `./mill mill.bsp.BSP/install` as mentioned above.





### Run Tests

```bash
./mill __.test
```

or in watch mode:

```bash
./mill -w __.test
```

### Formatting Code

Code needs to be formatted according to `scalafmt` rules. To run `scalafmt` on all the source code using:

```bash
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

or the much shorter:

```bash
./mill reformatAll __.sources
```



or in watch mode to reformat changed files:

```bash
./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

#

### IntelliJ Setup for Windows

If you are using IntelliJ IDEA to edit morphir-scala's Scala code, you can create the
IntelliJ project files via or use the **BSP Setup** option (BSP is the recommended approach):

```
.\mill -i mill.scalalib.GenIdea/idea
```

### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```
.\mill mill.bsp.BSP/install
```

You can also try the following to use the `mill-contrib-bloop` plugin to install and use the Bloop based BSP setup.

> NOTE: The recommended BSP based setup for mill is to run: `.\mill mill.bsp.BSP/install` as mentioned above.

```
.\mill -i --import ivy:com.lihaoyi::mill-contrib-bloop:  mill.contrib.bloop.Bloop/install
```

### Run Tests

```
.\mill -i __.test
```

or in watch mode:

```
.\mill -i -w __.test
```

### Formatting Code

Code needs to be formatted according to `scalafmt` rules. To run `scalafmt` on all the source code using:

```
.\mill -i mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

or in watch mode to reformat changed files:

```
.\mill -i -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

### Building website

```
./mill -i __.docusaurusBuild
```

### Running website locally
```
./mill -i __.docusaurusServe 
```


## Roadmap

Watch for updates.

## Contributing

1. Fork it (<https://github.com/finos/morphir-scala/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Read our [contribution guidelines](.github/CONTRIBUTING.md) and [Community Code of Conduct](https://www.finos.org/code-of-conduct)
4. Commit your changes (`git commit -am 'Add some fooBar'`)
5. Push to the branch (`git push origin feature/fooBar`)
6. Create a new Pull Request

_NOTE:_ Commits and pull requests to FINOS repositories will only be accepted from those contributors with an active, executed Individual Contributor License Agreement (ICLA) with FINOS OR who are covered under an existing and active Corporate Contribution License Agreement (CCLA) executed with FINOS. Commits from individuals not covered under an ICLA or CCLA will be flagged and blocked by the FINOS Clabot tool (or [EasyCLA](https://github.com/finos/community/blob/master/governance/Software-Projects/EasyCLA.md)). Please note that some CCLAs require individuals/employees to be explicitly named on the CCLA.

*Need an ICLA? Unsure if you are covered under an existing CCLA? Email [help@finos.org](mailto:help@finos.org)*


## License

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
