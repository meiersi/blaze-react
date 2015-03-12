{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Blaze.Core.Examples.TimeMachine    as TimeMachine
import qualified Blaze.Core.Examples.SocketTest     as SocketTest

import qualified Blaze.ReactJS.Examples.TimeMachine as TimeMachine
import qualified Blaze.ReactJS.Examples.SocketTest  as SocketTest
import qualified Blaze.ReactJS.Service.Socket       as Socket

import qualified Blaze.ReactJS.Run                  as ReactJS

import           Control.Applicative ((<$>))

main :: IO ()
main = do
    sock <- Socket.newSocket
    ReactJS.runApp' renderState (handleRequest sock <$> app)
  where
    app           = TimeMachine.wrapApp       SocketTest.app
    handleRequest = TimeMachine.wrapHandler . SocketTest.handleRequest
    renderState   = TimeMachine.wrapRenderer  SocketTest.renderState

