module Test.Main where

import Prelude

import Effect (Effect)
import Test.README (authUsage, rootedUsage)

main ∷ Effect Unit
main = do
  authUsage
  rootedUsage
