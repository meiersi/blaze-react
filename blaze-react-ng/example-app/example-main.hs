{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Blaze2.Core.Examples.TimeMachine    as TimeMachine
import qualified Blaze2.Core.Examples.SocketTest     as SocketTest

import qualified Blaze2.ReactJS.Examples.TimeMachine as TimeMachine
import qualified Blaze2.ReactJS.Examples.SocketTest  as SocketTest
import qualified Blaze2.ReactJS.Service.Socket       as Socket

import qualified Blaze2.ReactJS.Run                  as ReactJS

main :: IO ()
main = do
    sock <- Socket.newSocket
    ReactJS.runApp' (TimeMachine.wrapApp       SocketTest.app)
                    (TimeMachine.wrapRenderer  SocketTest.renderState)
                    (TimeMachine.wrapHandler $ SocketTest.handleRequest sock)

