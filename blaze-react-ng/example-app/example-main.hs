{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Blaze2.Core.Examples.Todo    as Todo
import qualified Blaze2.ReactJS.Examples.Todo as ReactJS.Todo
import qualified Blaze2.ReactJS.Run           as ReactJS

import           Data.IORef

main :: IO ()
main = do
    ref <- newIORef [Todo.TodoItem False "It works!"]
    ReactJS.runApp' Todo.app ReactJS.Todo.renderState (ReactJS.Todo.handleRequest ref)


