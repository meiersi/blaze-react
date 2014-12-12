{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Blaze.React

import qualified Blaze.React.Examples.Clock       as Clock
import qualified Blaze.React.Examples.EventTest   as EventTest
import qualified Blaze.React.Examples.RoutingTest as RoutingTest
import qualified Blaze.React.Examples.Todo        as Todo
import           Blaze.React.Examples.TabbedApps  (namedApp, namedApp', tabbed)
import           Blaze.React.Examples.TimeMachine (withTimeMachine)
import           Blaze.React.Examples.MultiUser   (withMultiUser)
import qualified Blaze.React.Run.ReactJS          as ReactJS


main :: IO ()
main = ReactJS.runApp $
    tabbed 0
      [ namedApp' "TodoMVC" Todo.app
      , namedApp "TodoMVC (with TimeMachine™)" $ withTimeMachine $ ignoreWindowActions Todo.app
      , namedApp "Multi-user TodoMVC" $ withTimeMachine $ withMultiUser $ ignoreWindowActions Todo.app
      , namedApp' "Clock" Clock.app
      , namedApp' "Event test" EventTest.app
      , namedApp  "Routing test"                $ withTimeMachine RoutingTest.app
      ]
