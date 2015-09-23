{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Blaze.Core
import           Blaze.ReactJS.Base                 (WindowState)
import qualified Blaze.ReactJS.Run                  as ReactJS

import           Blaze.Core.Examples.TabbedApps
                    (TabbedS, TabbedA, TabbedR, SomeApp(..))
import           Blaze.ReactJS.Examples.TabbedApps
                    (SomeHandler(..), SomeRenderer(..))

import qualified Blaze.Core.Examples.TabbedApps     as TabbedApps
import qualified Blaze.Core.Examples.TimeMachine    as TimeMachine
import qualified Blaze.Core.Examples.Todo           as Todo
import qualified Blaze.Core.Examples.SocketTest     as SocketTest
import qualified Blaze.Core.Service.Socket          as Socket

import qualified Blaze.ReactJS.Examples.TabbedApps  as TabbedApps
import qualified Blaze.ReactJS.Examples.TimeMachine as TimeMachine
import qualified Blaze.ReactJS.Examples.Todo        as Todo
import qualified Blaze.ReactJS.Examples.SocketTest  as SocketTest
import qualified Blaze.ReactJS.Service.Socket       as Socket

import           Control.Applicative ((<$>))

main :: IO ()
main = do
    sock <- Socket.newSocket
    ReactJS.runApp' render (handle sock <$> app)

app :: App TabbedS TabbedA TabbedR
app = TabbedApps.wrapApps
    [ SomeApp "To-do" $ TimeMachine.wrapApp Todo.app
    , SomeApp "Socket" SocketTest.app
    ]

handle :: Socket.Socket -> TabbedR -> (TabbedA -> IO ()) -> IO ()
handle sock = TabbedApps.wrapHandlers
    [ SomeHandler $ TimeMachine.wrapHandler $ Todo.handleRequest "blaze-react-todo-mvc"
    , SomeHandler $ SocketTest.handleRequest sock
    ]

render :: TabbedS -> WindowState TabbedA
render = TabbedApps.wrapRenderers
    [ SomeRenderer foo
    , SomeRenderer SocketTest.renderState
    ]
  where
    foo :: TimeMachine.TMState Todo.TodoS Todo.TodoA Todo.TodoR-> WindowState (TimeMachine.TMAction Todo.TodoA Todo.TodoR)
    foo = TimeMachine.wrapRenderer Todo.renderState
