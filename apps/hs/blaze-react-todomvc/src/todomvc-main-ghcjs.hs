{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Blaze.React.Examples.Todo        as Todo
import qualified Blaze.React.Backend.ReactJS.Run  as ReactJS


main :: IO ()
main = ReactJS.runApp' Todo.render (const noRequest <$> Todo.app)
  where
    noRequest _submitAct = return ()

