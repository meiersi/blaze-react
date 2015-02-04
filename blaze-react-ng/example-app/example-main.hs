{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Blaze2.Core.Examples.TimeMachine    as TimeMachine
import qualified Blaze2.Core.Examples.Todo           as Todo

import qualified Blaze2.ReactJS.Examples.TimeMachine as TimeMachine
import qualified Blaze2.ReactJS.Examples.Todo        as Todo

import qualified Blaze2.ReactJS.Run                  as ReactJS

main :: IO ()
main =
    ReactJS.runApp' (TimeMachine.wrapApp       Todo.app)
                    (TimeMachine.wrapRenderer  Todo.renderState)
                    (TimeMachine.wrapHandler $ Todo.handleRequest "todomvc")

