#!/usr/bin/env bash

set -eu

echo "$SONATYPE_PGP_SECRET" | base64 --decode > gpg_key

gpg --import  --no-tty --batch --yes gpg_key

rm gpg_key

# Build all artifacts
./mill -i __.publishArtifacts

# Publish all artifacts
# Updated to use Sonatype Central Portal compatibility API
# See: https://central.sonatype.org/publish/publish-portal-ossrh-staging-api/
./mill -i \
    mill.scalalib.PublishModule/publishAll \
    --sonatypeCreds "$SONATYPE_DEPLOY_USER":"$SONATYPE_DEPLOY_PASSWORD" \
    --gpgArgs --passphrase="$SONATYPE_PGP_PASSWORD",--no-tty,--pinentry-mode,loopback,--batch,--yes,-a,-b \
    --publishArtifacts __.publishArtifacts \
    --readTimeout  3600000 \
    --awaitTimeout 3600000 \
    --release true \
    --signed  true \
    --sonatypeUri https://ossrh-staging-api.central.sonatype.com/service/local \
    --sonatypeSnapshotUri https://central.sonatype.com/repository/maven-snapshots
