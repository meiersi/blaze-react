{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | An @HTML@ empty data type with `MimeRender` instances for the
-- `blaze-react`'s  'H.ToMarkup' class and 'H.Html' datatype.
-- You should only need to import this module for it's instances and the
-- `HTML` datatype.:
--
-- >>> type Eg = Get '[HTML] a
--
-- Will then check that @a@ has a 'H.ToMarkup' instance, or is 'H.Html'.
-- See
-- <http://haskell-servant.github.io/tutorial/server.html#case-studies-servant-blaze-and-servant-lucid>
-- for more information.
--
module Servant.HTML.BlazeReact
  ( HTML
  ) where

import qualified Data.ByteString.Lazy          as BL
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Typeable                 (Typeable)

import qualified Network.HTTP.Media            as M

import           Servant.API                   (Accept (..), MimeRender (..))

import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Renderer.String

data HTML deriving Typeable

-- | @text/html;charset=utf-8@
instance Accept HTML where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance
    MimeRender HTML (H.Html ev)
  where
    mimeRender _ =
        BL.fromStrict . TE.encodeUtf8 . T.pack
      -- TODO (SM): move doctype support into Html datatype itself.
      . ("<!DOCTYPE html>" <>)
      . Text.Blaze.Renderer.String.renderHtml

