#! /bin/sh

if [ -n "$1" ]
then
    # HACK (SM): use ghcid whenever an argument is passed
    ghcid --command="stack ghci --main-is=blaze-react-todomvc:blaze-react-todomvc" \
          --test="Main.main"
else
    # and stack otherwise
    stack ghci
fi

