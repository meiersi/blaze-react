#! /bin/sh

if [ -n "$1" ]
then
    # HACK (SM): use ghcid whenever an argument is passed
    ghcid --command="stack ghci" \
          --test="Blaze.Development.Server.testMain"
else
    # and stack otherwise
    stack ghci
fi

