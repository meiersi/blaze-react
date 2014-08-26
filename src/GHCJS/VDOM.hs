{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, QuasiQuotes, FlexibleInstances, EmptyDataDecls, TypeFamilies, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

{-
  A low-level binding to the virtual-dom library used by a modified version of
  blaze-html to build client-side web-apps.

  warning, unfinished!

  a proof of concept integration with the virtual-dom library.

  The diff function has been changed slightly to allow it to work with full
  functionality in asynchronous threads. It's possible to implement the bindings
  without the modifications at the cost of tail-call optimization and
  preemptive threading in the diff, by recursively forcing the thunks
  in synchronous threads.

 -}

module GHCJS.VDOM
    (
      -- * Virtual dom construction
      VNode
    , text
    , vnode

    , Properties
    , newProperties
    , setProperty
    , setAttributes

    , Children
    , newChildren
    , pushChild

      -- * Diffing and patch application
    , DOMNode
    , diff
    , applyPatch

    ) where


import           Control.Applicative

import           Prelude hiding (div)

import           GHCJS.Types
import           GHCJS.Foreign.QQ
import qualified GHCJS.Foreign      as Foreign
import           GHCJS.Prim

import           Control.Exception
import           Control.Monad

import           System.IO.Unsafe
import           Unsafe.Coerce


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

newtype Properties = Properties { _unProperties :: JSObject JSString }
newtype Children   = Children   { _unChildren   :: JSArray VNode }
newtype VNode      = VNode      { _unVNode      :: JSRef () }
newtype Patch      = Patch      { _unPatch      :: JSRef () }

data DOMNode_
type DOMNode = JSRef DOMNode_



------------------------------------------------------------------------------
-- VirtualDom Construction
------------------------------------------------------------------------------

vnode :: JSString -> Properties -> Children -> VNode
vnode tag (Properties props) (Children children) =
  VNode [jsu'| new h$vdom.VNode(`tag, `props, `children) |]

text :: JSString -> VNode
text xs = VNode [jsu'| new h$vdom.VText(`xs) |]


-- IO-based constructino of children and properties
---------------------------------------------------

newChildren :: IO Children
newChildren = Children <$> Foreign.newArray

pushChild :: VNode -> Children -> IO ()
pushChild (VNode node) (Children children) =
    Foreign.pushArray node children

newProperties :: IO Properties
newProperties = Properties <$> Foreign.newObj

setProperty :: JSString -> JSString -> Properties -> IO ()
setProperty key value (Properties properties) =
    Foreign.setProp key value properties

-- | Set the attributes to a single key value pair.
-- FIXME (SM): make this more generic and less surprising. It is what we just
-- need for the blaze-html renderer.
setAttributes :: JSString -> JSString -> Properties -> IO ()
setAttributes key value (Properties properties) = do
    attributes <- Foreign.newObj
    Foreign.setProp key value attributes
    Foreign.setProp ("attributes" :: JSString) attributes properties


------------------------------------------------------------------------------
-- Diffing and patching
------------------------------------------------------------------------------

diff :: VNode -> VNode -> Patch
diff (VNode a) (VNode b) = Patch (unsafePerformIO $ diff' a b)

diff' :: JSRef () -> JSRef () -> IO (JSRef ())
diff' a b = do
    thunks <- [jsu| [] |]
    patch  <- [js| h$vdom.diff(`a, `b, `thunks) |]
    when [jsu'| `thunks.length > 0|] (forceThunks thunks)
    forcePatch patch
    return patch

forceThunks :: JSRef () -> IO ()
forceThunks =
    fromJSArray >=> mapM_ forceNode
  where
    forceNode n = do
      forceThunkNode [jsu'| `n.a |]
      forceThunkNode [jsu'| `n.b |]
      patch <- diff' [jsu'| `n.a.vnode |] [jsu'| `n.b.vnode |]
      [jsu_| h$vdom.setThunkPatch(`n, `patch); |]

foreign import javascript unsafe "$1.hst" getThunk :: JSRef () -> IO Double

forceThunkNode :: JSRef () -> IO ()
forceThunkNode x
  | [jsu'| `x && `x.hst |] = do
      (h::Double) <- getThunk x
      let (Just (t::VNode)) = unsafeCoerce h
      (VNode u) <- evaluate t
      [jsu| `x.hst = null; `x.vnode = `u; |]
  | otherwise = return ()

forcePatch :: JSRef () -> IO ()
forcePatch p = do
  thunks <- [jsu| h$vdom.forcePatch(`p) |]
  forceTree [thunks]

forceTree :: [JSRef ()] -> IO ()
forceTree [] = return ()
forceTree (x:xs) = do
  x' <- fromJSArray x
  ys <- forM x' $ \t -> do
    forceThunkNode t
    newThunks <- [jsu| [] |]
    [jsu_| h$vdom.forceTree(`t.vnode, `newThunks) |]
    return newThunks
  forceTree (filter (\a -> [jsu'| `a.length !== 0 |]) ys ++ xs)

applyPatch :: DOMNode -> Patch -> IO ()
applyPatch n (Patch p) = [js| h$vdom.patch(`n, `p); |]

