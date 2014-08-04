module GHCJS.VDOM.Internal where

import Language.Haskell.TH.Quote

import GHCJS.Foreign.QQ
import GHCJS.Types

-- do not export the constructors for these, this ensures that the objects are opaque
-- and cannot be mutated
newtype Properties = Properties { unProperties :: JSRef () }
newtype Children   = Children   { unChildren   :: JSRef () }
newtype VNode      = VNode      { unVNode      :: JSRef () }
newtype Patch      = Patch      { unPatch      :: JSRef () }

data JSIdent_
type JSIdent = JSRef JSIdent_
data DOMNode_
type DOMNode = JSRef DOMNode_

type J = JSRef ()

j :: QuasiQuoter
j = jsu'


