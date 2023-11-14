#!/usr/bin/env sh
cs fetch org.finos.morphir:morphir-main_3:latest.release
cs bootstrap org.finos.morphir:morphir-main_3:latest.release -M org.finos.morphir.cli.MorphirCliMain -f -o  ~/Library/Application\ Support/Coursier/bin/morphir-cli
