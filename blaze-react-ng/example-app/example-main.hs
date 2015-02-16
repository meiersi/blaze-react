{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Blaze2.Core.Examples.TimeMachine    as TimeMachine
import qualified Blaze2.Core.Examples.SocketTest     as SocketTest

import qualified Blaze2.ReactJS.Examples.TimeMachine as TimeMachine
import qualified Blaze2.ReactJS.Examples.SocketTest  as SocketTest
import qualified Blaze2.ReactJS.Service.Socket       as Socket

import qualified Blaze2.ReactJS.Run                  as ReactJS

import           Control.Applicative ((<$>))

main :: IO ()
main = do
    sock <- Socket.newSocket
    ReactJS.runApp' renderState (handleRequest sock <$> app)
  where
    app           = TimeMachine.wrapApp       SocketTest.app
    handleRequest = TimeMachine.wrapHandler . SocketTest.handleRequest
    renderState   = TimeMachine.wrapRenderer  SocketTest.renderState

