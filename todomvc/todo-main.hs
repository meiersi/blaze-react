module Main (main) where

import qualified Blaze.React.Examples.Todo        as Todo
import           Blaze.React.Examples.TimeMachine (withTimeMachine)
import qualified Blaze.React.Run.ReactJS          as ReactJS


main :: IO ()
-- main = ReactJS.runApp Todo.app
main = ReactJS.runApp (withTimeMachine Todo.app)


