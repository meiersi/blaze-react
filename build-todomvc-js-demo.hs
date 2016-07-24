#!/bin/bash

set -e

USE_GHCJS="--stack-yaml=stack-ghcjs.yaml"
TARGET_DIR="dist/blaze-react-todomvc-example"
SOURCE_DIR_SUFFIX="bin/"

# Build the client
stack build $USE_GHCJS --pedantic

# Copy over the javascript
rm -rf $TARGET_DIR/*
mkdir -p $TARGET_DIR
cp $(stack path $USE_GHCJS --local-install-root)/bin/blaze-react-todomvc.jsexe/*.js $TARGET_DIR
rsync -av apps/hs/blaze-react-todomvc/web-assets/* $TARGET_DIR
rm $TARGET_DIR/all.js

echo ""
echo "---"
echo "Build completed: open $TARGET_DIR/index.html to load the demo."
echo "---"
