[![FINOS - Incubating](https://cdn.jsdelivr.net/gh/finos/contrib-toolbox@master/images/badge-incubating.svg)](https://finosfoundation.atlassian.net/wiki/display/FINOS/Incubating)
[<img src="https://img.shields.io/badge/slack-@finos/morphir-green.svg?logo=slack">](https://finos-lf.slack.com/messages/morphir/)

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

```bash
./mill mill.contrib.Bloop/install
```

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

or in watch mode to reformat changed files:

```bash
./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

#

### IntelliJ Setup for Windows

If you are using IntelliJ IDEA to edit morphir-jvm's Scala code, you can create the
IntelliJ project files via:

```
.\mill -i mill.scalalib.GenIdea/idea
```

### BSP Setup

If you are using Visual Studio Code, IntelliJ, or any of the Editors which support BSP you can also generate BSP config files via:

```
.\mill -i mill.contrib.Bloop/install
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
