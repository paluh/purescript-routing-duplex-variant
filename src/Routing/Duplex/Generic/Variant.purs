module Routing.Duplex.Generic.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, case_, inj, on)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row (class Cons)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', prefix)
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Printer (RoutePrinter)
import Type.Prelude (SProxy, reflectSymbol)

prs ∷ ∀ a. RouteDuplex' a → RouteParser a
prs (RouteDuplex _ p) = p

prt ∷ ∀ a. RouteDuplex' a → (a → RoutePrinter)
prt (RouteDuplex p _) = p

data VariantParser = VariantParser

instance variantParserUnit ::
  (IsSymbol sym, Cons sym a v' v) =>
  FoldingWithIndex VariantParser (SProxy sym) Unit (RouteDuplex a a) (RouteParser (Variant v))
  where
  foldingWithIndex _ prop _ a = inj prop <$> prs a

instance variantParserParser ::
  (IsSymbol sym, Cons sym a v' v) =>
  FoldingWithIndex VariantParser (SProxy sym) (RouteParser (Variant v)) (RouteDuplex a a) (RouteParser (Variant v))
  where
  foldingWithIndex _ prop prev a = prev <|> (inj prop <$> (prs a))

data VariantPrinter = VariantPrinter

instance variantPrinterUnit ::
  (IsSymbol sym, Cons sym a () v) =>
  FoldingWithIndex VariantPrinter (SProxy sym) Unit (RouteDuplex a a) (Variant v → RoutePrinter)
  where
  foldingWithIndex _ prop _ d = case_ # on prop (prt d)

instance variantPrinterPrinter ::
  (IsSymbol sym, Cons sym a v' v) =>
  FoldingWithIndex VariantPrinter (SProxy sym) (Variant v' → RoutePrinter) (RouteDuplex a a) (Variant v → RoutePrinter)
  where
  foldingWithIndex _ prop prev d = prev # on prop (prt d)

data PrefixRoutes = PrefixRoutes

-- | Similar to `Route.Duplex.Generic.sum` but for Variant types.
-- |
-- |  ```purescript
-- |  rd ∷ RouteDuplex'
-- |    (Variant
-- |      ( login ∷ String
-- |      , register ∷ Int
-- |      )
-- |   )
-- |  rd = variant { "register": int segment , "login": string segment }
-- |  ```
variant
  ∷ ∀ r v
  . HFoldlWithIndex VariantParser Unit (Record r) (RouteParser (Variant v))
  ⇒ HFoldlWithIndex VariantPrinter Unit (Record r) (Variant v → RoutePrinter)
  ⇒ Record r
  → RouteDuplex' (Variant v)
variant routes = RouteDuplex printer parser
  where
    printer = hfoldlWithIndex VariantPrinter unit routes
    parser = hfoldlWithIndex VariantParser unit routes

instance prefixRoutes ::
  (IsSymbol sym) =>
  MappingWithIndex PrefixRoutes (SProxy sym) (RouteDuplex a a) (RouteDuplex a a)
  where
  mappingWithIndex _ prop = prefix (reflectSymbol prop)

-- | Same as `variant` but also sets url prefix based on the label from the given field.
variant'
  ∷ ∀ r v
  . HFoldlWithIndex VariantParser Unit (Record r) (RouteParser (Variant v))
  ⇒ HFoldlWithIndex VariantPrinter Unit (Record r) (Variant v → RoutePrinter)
  ⇒ HMapWithIndex PrefixRoutes (Record r) (Record r)
  ⇒ Record r
  → RouteDuplex' (Variant v)
variant' routes = variant (hmapWithIndex PrefixRoutes routes ∷ Record r)
