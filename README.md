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
import Routing.Duplex (RouteDuplex(..), RouteDuplex', int, parse, print, root, segment, string)
import Routing.Duplex.Generic.Variant (variant')
import Routing.Duplex.Parser (end) as Parser
import Test.Assert (assert)
import Type.Prelude (Proxy(..))

```

Now we can define some (not really practical) routes for tesing purposes and use them:

```purescript
authDuplex :: RouteDuplex'
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
    (print authDuplex (inj (Proxy ∷ Proxy "register") "user1"))
    "register/user1"

  assert $ eq
    (print authDuplex (inj (Proxy ∷ Proxy "login") 8))
    "login/8"

  assert $ eq
    (parse authDuplex "register/user2")
    (Right (inj (Proxy :: Proxy "register") "user2"))

  assert $ eq
    (parse authDuplex "login/22")
    (Right (inj (Proxy :: Proxy "login") 22))
```

## Handling root path with `variant'`

You can use `variant'` combinator toghether with emtpy root path to get proper routing.

We could define here a dedicated `Root` data type and duplex for it here. I'm using the `Unit` type with a bit cheaty duplex to minimize the boilerplate.

```purescript

lastUnitDuplex ∷ RouteDuplex' Unit
lastUnitDuplex = RouteDuplex mempty (Parser.end *> pure unit)

rootedDuplex :: RouteDuplex'
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
    (print rootedDuplex (inj (Proxy ∷ Proxy "auth") $ inj (Proxy ∷ Proxy "register") $ "user2"))
    "/auth/register/user2"

  assert $ eq
    (print rootedDuplex (inj (Proxy ∷ Proxy "") unit))
    "/"

  assert $ eq
    (parse rootedDuplex "/auth/register/user2")
    (Right $ inj (Proxy ∷ Proxy "auth") $ inj (Proxy ∷ Proxy "register") $ "user2")

  assert $ eq
    (parse rootedDuplex "/")
    (Right $ inj (Proxy ∷ Proxy "") $ unit)
```

## Credits
This repo is just adaptation of the sum generic handling code from the original _purescript-routing-duplex_ by @natefaubion.
