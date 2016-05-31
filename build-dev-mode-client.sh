#!/bin/bash

set -e

USE_GHCJS="--stack-yaml=stack-ghcjs.yaml"
TARGET_DIR="libs/generated/js/blaze-react-dev-mode-client"
SOURCE_DIR_SUFFIX="bin/"

# Build the client
stack build $USE_GHCJS

# Copy over the javascript
rm -f $TARGET_DIR/*
cp $(stack path $USE_GHCJS --local-install-root)/bin/dev-mode-client.jsexe/*.js $TARGET_DIR
rm $TARGET_DIR/all.js
