{-# LANGUAGE TemplateHaskell #-}

-- | Separate file providing embedded assets.
module Blaze.Development.Server.Assets
  ( staticFiles
  ) where


import qualified Data.ByteString              as B
import           Data.FileEmbed               (embedDir)


staticFiles :: [(FilePath, B.ByteString)]
staticFiles = $(embedDir "blaze-react-dev-mode-server-assets")
