module Test.Main where

import Prelude

import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Class.Console (log)
import Routing.Duplex (RouteDuplex', int, parse, print, segment, string)
import Routing.Duplex.Generic.Variant (variant')
import Type.Prelude (SProxy(..))

x :: RouteDuplex'
  (Variant
     ( login :: String
     , register :: Int
     )
  )
x = variant'
  { "register": int segment
  , "login": string segment
  }

main ∷ Effect Unit
main = do
  log $ print x (inj (SProxy ∷ SProxy "register") 8)
  log $ print x (inj (SProxy ∷ SProxy "login") "test")

  log $ show $ parse x ("register/888")
  log $ show $ parse x ("login/test")
