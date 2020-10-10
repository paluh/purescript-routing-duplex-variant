module Routing.Duplex.Generic.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, case_, inj, on)
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, kind RowList)
import Record (get) as Record
import Record.Builder (build, Builder) as Record.Builder
import Record.Unsafe (unsafeSet)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', prefix)
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Printer (RoutePrinter)
import Type.Prelude (RLProxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

prs ∷ ∀ a. RouteDuplex' a → RouteParser a
prs (RouteDuplex _ p) = p

prt ∷ ∀ a. RouteDuplex' a → (a → RoutePrinter)
prt (RouteDuplex p _) = p

class VariantParser (rl ∷ RowList) (routes ∷ # Type) (variantRoute ∷ #Type) | routes → variantRoute where
  variantParser ∷ RLProxy rl → Record routes → RouteParser (Variant variantRoute)

instance variantParserNil ::
  (IsSymbol sym, Row.Cons sym (RouteDuplex' a) r' r, Row.Cons sym a v' v) =>
  VariantParser (RowList.Cons sym (RouteDuplex a a) RowList.Nil) r v
  where
    variantParser _ r = inj prop <$> prs (Record.get prop r)
      where
        prop = SProxy ∷ SProxy sym

else instance variantParserCons ∷
  (IsSymbol sym, VariantParser tail r v, Row.Cons sym (RouteDuplex' a) r' r, Row.Cons sym a v' v) =>
  VariantParser (RowList.Cons sym (RouteDuplex a a) tail) r v
  where
    variantParser _ r = inj prop <$> prs (Record.get prop r) <|> variantParser (RLProxy ∷ RLProxy tail) r
      where
        prop = SProxy ∷ SProxy sym

class VariantPrinter (rl ∷ RowList) (routes ∷ # Type) (variantRoute ∷ #Type) | rl → routes, rl → variantRoute where
  variantPrinter ∷ RLProxy rl → Record routes → Variant variantRoute → RoutePrinter

instance variantPrinterNil ::
  VariantPrinter RowList.Nil r ()
  where
    variantPrinter _ _ = case_

else instance variantPrinterCons ∷
  (IsSymbol sym, VariantPrinter tail r v', Row.Cons sym (RouteDuplex' a) r' r, Row.Cons sym a v' v) =>
  VariantPrinter (RowList.Cons sym (RouteDuplex a a) tail) r v
  where
    variantPrinter _ r = variantPrinter (RLProxy ∷ RLProxy tail) r # on prop (prt (Record.get prop r))
      where
        prop = SProxy ∷ SProxy sym

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
  ∷ ∀ r rl v
  . RowToList r rl
  ⇒ VariantParser rl r v
  ⇒ VariantPrinter rl r v
  ⇒ Record r
  → RouteDuplex' (Variant v)
variant routes = RouteDuplex printer parser
  where
    printer = variantPrinter (RLProxy ∷ RLProxy rl) routes
    parser = variantParser (RLProxy ∷ RLProxy rl) routes

-- | In place record updater. Provided to simplify
-- | PrefixRoutes typeclass signatures
newtype Updater r = Updater (Record.Builder.Builder r r)

instance semigroupUpdater ∷ Semigroup (Updater r) where
  append (Updater b1) (Updater b2) = Updater (b1 <<< b2)

instance monoidUpdater ∷ Monoid (Updater r) where
  mempty = Updater identity

modify ∷ ∀ a r r' sym. IsSymbol sym ⇒ Row.Cons sym a r' r ⇒ SProxy sym → (a → a) → Updater (Record r)
modify prop f = Updater modify'
  where
    modify' ∷ Record.Builder.Builder (Record r) (Record r)
    modify' = unsafeCoerce unsafeModify

    unsafeModify ∷ Record r → Record r
    unsafeModify r =
      let a = Record.get prop r
      in unsafeSet (reflectSymbol prop) (f a) r

update ∷ ∀ r. Updater (Record r) → Record r → Record r
update (Updater b) r = Record.Builder.build b r

class PrefixRoutes (rl ∷ RowList) routes where
  prefixRoutes ∷ RLProxy rl → Updater { | routes }

instance prefixRoutesNil ∷ PrefixRoutes RowList.Nil routes where
  prefixRoutes _ = mempty

instance prefixRoutesEmptyCons ::
  (PrefixRoutes tail routes, Row.Cons "" (RouteDuplex a a) r' routes) =>
  PrefixRoutes (RowList.Cons "" (RouteDuplex a a) tail) routes
  where
    prefixRoutes _ = prefixRoutes (RLProxy ∷ RLProxy tail)

else instance prefixRoutesCons ::
  (IsSymbol sym, PrefixRoutes tail routes, Row.Cons sym (RouteDuplex a a) r' routes) =>
  PrefixRoutes (RowList.Cons sym (RouteDuplex a a) tail) routes
  where
    prefixRoutes _ = modify prop (prefix (reflectSymbol prop)) <> prefixRoutes (RLProxy ∷ RLProxy tail)
      where
        prop = SProxy ∷ SProxy sym

class (VariantParser rl r v, VariantPrinter rl r v, PrefixRoutes rl r) ⇐ Variant' (rl ∷ RowList) (r ∷ # Type) (v ∷ # Type)
instance variantParser' ∷ (VariantParser rl r v, VariantPrinter rl r v, PrefixRoutes rl r) ⇒ Variant' rl r v

-- | Same as `variant` but also sets url prefix based on the label from the given field.
variant'
  ∷ ∀ r rl v
  . RowToList r rl
  ⇒ Variant' rl r v
  ⇒ Record r
  → RouteDuplex' (Variant v)
variant' routes = variant (update (prefixRoutes (RLProxy ∷ RLProxy rl)) routes)
