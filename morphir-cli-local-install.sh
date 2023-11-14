#!/usr/bin/env sh
echo "Running ./mill __.publishLocal"

# local publishing
command=$(./mill __.publishLocal 2>&1)
echo "$command"
snapshot_version=$(echo "$command" | grep morphir-main | awk '{print $2}' | sed -n 's/.*Artifact([^,]*,[^,]*,\(.*\)).*/\1/p')
echo "Published local version: $snapshot_version"

cs bootstrap org.finos.morphir:morphir-main_3:${snapshot_version} -M org.finos.morphir.cli.MorphirCliMain -f -o  ~/Library/Application\ Support/Coursier/bin/morphir-cli-local

