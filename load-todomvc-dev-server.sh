#! /bin/sh

ghcid --command="stack ghci --main-is=blaze-react-todomvc:blaze-react-todomvc" \
      --test="Main.main"

