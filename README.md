# purescript-routing-duplex-variant

Build variant duplex from a given record. It is like `Routing.Duplex.Generic.sum` but for `Variant` based types.

```purescript
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
  -- | output: /register/8
  log $ print x (inj (SProxy ∷ SProxy "register") 8)
  -- | output: /login/test
  log $ print x (inj (SProxy ∷ SProxy "login") "test")

  -- | output: Right (inj @register 888)
  log $ show $ parse x ("register/888")
  -- | output: Right (inj @test "test")
  log $ show $ parse x ("login/test")
```
