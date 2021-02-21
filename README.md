# purescript-routing-duplex-variant

Build variant duplex from a given record. It is like `Routing.Duplex.Generic.sum` but for `Variant` based types.

## Usage

Let me start with imports. This is a literate PureScript example (run as a part of the test suite) so we need them.

```purescript
module Test.README where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Request.Duplex (RequestDuplex(..), RequestDuplex', int, parse, print, root, segment, string)
import Request.Duplex.Generic.Variant (variant')
import Request.Duplex.Parser (end) as Parser
import Request.Duplex.Types (Method(Get))
import Test.Assert (assert)
import Type.Prelude (SProxy(..))

```

Now we can define some (not really practical) routes for tesing purposes and use them:

```purescript

req url = { method: Get, url }

authDuplex :: RequestDuplex'
  (Variant
     ( login :: Int
     , register :: String
     )
  )
authDuplex = variant'
  { "login": int segment
  , "register": string segment
  }

authUsage ∷ Effect Unit
authUsage = do
  assert $ eq
    (print authDuplex (inj (SProxy ∷ SProxy "register") "user1"))
    (req "register/user1")

  assert $ eq
    (print authDuplex (inj (SProxy ∷ SProxy "login") 8))
    (req "login/8")

  assert $ eq
    (parse authDuplex (req "register/user2"))
    (Right (inj (SProxy :: SProxy "register") "user2"))

  assert $ eq
    (parse authDuplex (req "login/22"))
    (Right (inj (SProxy :: SProxy "login") 22))
```

## Handling root path with `variant'`

You can use `variant'` combinator toghether with emtpy root path to get proper routing.

We could define here a dedicated `Root` data type and duplex for it here. I'm using the `Unit` type with a bit cheaty duplex to minimize the boilerplate.

```purescript

lastUnitDuplex ∷ RequestDuplex' Unit
lastUnitDuplex = RequestDuplex mempty (Parser.end *> pure unit)

rootedDuplex :: RequestDuplex'
  (Variant
     ( "" :: Unit
     , "auth" :: Variant (login :: Int, register :: String)
     )
  )
rootedDuplex = root $ variant'
  { "": lastUnitDuplex
  , "auth": authDuplex
  }

rootedUsage ∷ Effect Unit
rootedUsage = do
  assert $ eq
    (print rootedDuplex (inj (SProxy ∷ SProxy "auth") $ inj (SProxy ∷ SProxy "register") $ "user2"))
    (req "/auth/register/user2")

  assert $ eq
    (print rootedDuplex (inj (SProxy ∷ SProxy "") unit))
    (req "/")

  assert $ eq
    (parse rootedDuplex $ req "/auth/register/user2")
    (Right $ inj (SProxy ∷ SProxy "auth") $ inj (SProxy ∷ SProxy "register") $ "user2")

  assert $ eq
    (parse rootedDuplex $ req "/")
    (Right $ inj (SProxy ∷ SProxy "") $ unit)
```
