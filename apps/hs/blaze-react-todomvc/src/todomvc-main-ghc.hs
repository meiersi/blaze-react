{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Blaze.React.Examples.Todo        as Todo


import qualified Blaze.React.Development.Server   as DS

main :: IO ()
main = do
    staticFiles <- DS.loadStaticFiles "apps/hs/blaze-react-todomvc/web-assets"
    let config = DS.defaultConfig [] staticFiles []
    DS.main DS.fullJsonEncoding config app
  where
    app = DS.RenderableApp Todo.render (const mempty <$> Todo.app)
