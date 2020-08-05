# purescript-routing-duplex-variant

Build variant duplex from a given record. It is like `Routing.Duplex.Generic.sum` but for `Variant` based types.

## Usage

Let me start with imports. This is a literate PureScript example (run as a part of the test suite) so we need them.

```purescript
module Test.README where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', int, parse, print, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum) as D
import Routing.Duplex.Generic.Variant (variant')
import Test.Assert (assert)
import Type.Prelude (SProxy(..))

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
    (print authDuplex (inj (SProxy ∷ SProxy "register") "user1"))
    "register/user1"

  assert $ eq
    (print authDuplex (inj (SProxy ∷ SProxy "login") 8))
    "login/8"

  assert $ eq
    (parse authDuplex "register/user2")
    (Right (inj (SProxy :: SProxy "register") "user2"))

  assert $ eq
    (parse authDuplex "login/22")
    (Right (inj (SProxy :: SProxy "login") 22))
```

## Handling root path with `variant'`

You can use `variant'` combinator toghether with emtpy root path to get proper routing.
Let's define `Root` 

```purescript

data Root = Root
derive instance genericRoot ∷ Generic Root _
derive instance eqRoot :: Eq Root

rootDuplex ∷ RouteDuplex' Root
rootDuplex = D.sum { "Root": D.noArgs }

rootedDuplex :: RouteDuplex'
  (Variant
     ( "" :: Root
     , "auth" :: Variant (login :: Int, register :: String)
     )
  )
rootedDuplex = root $ variant'
  { "": rootDuplex
  , "auth": authDuplex
  }

rootedUsage ∷ Effect Unit
rootedUsage = do
  assert $ eq
    (print rootedDuplex (inj (SProxy ∷ SProxy "auth") $ inj (SProxy ∷ SProxy "register") $ "user2"))
    "/auth/register/user2"

  assert $ eq
    (print rootedDuplex (inj (SProxy ∷ SProxy "") Root))
    "/"

  assert $ eq
    (parse rootedDuplex "/auth/register/user2")
    (Right $ inj (SProxy ∷ SProxy "auth") $ inj (SProxy ∷ SProxy "register") $ "user2")

  assert $ eq
    (parse rootedDuplex "/")
    (Right $ inj (SProxy ∷ SProxy "") $ Root)
```
