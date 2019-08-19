{-# LANGUAGE
    PolyKinds
  , MultiParamTypeClasses
  , DataKinds
  , FunctionalDependencies
  , GADTs
  , RankNTypes
  , TypeOperators
  , NoImplicitPrelude
  , TypeFamilies
#-}
module Main where

import Data.Proxy
import Data.Kind

type Hom k l = k -> k -> l

data Iso (p :: Hom pi po) (a :: pi) (b :: pi)
 where
 Iso ::
   { fwd :: p a b
   , bwd :: p b a
   } ->
   Iso p a b

data Monoidal (m :: Hom mi mo) (i :: mi) (t :: mi -> mi -> mi)
  where
  Monoidal ::
    { category :: Category m
    , assoc :: Iso m ((a `t` b) `t` c) (a `t` (b `t` c))
    , lunit :: Iso m (i `t` l) l
    , runit :: Iso m (r `t` i) r
    } ->
    Monoidal m i t

data Category (p :: Hom pi po)
  where
  Category ::
    { enrichment :: Monoidal m i t
    , identity :: i `m` p a a
    , compose :: (p y x `t` p x y) `m` p x z
    } ->
    Category p

hask :: Category (->)
hask = Category enrichment identity compose
  where
  identity _ x = x
  compose (f, g) x = f (g x)
  enrichment = Monoidal hask assoc lunit runit
    where
    assoc = Iso fwd bwd
      where
      fwd ((a, b), c) = (a, (b, c))
      bwd (a, (b, c)) = ((a, b), c)
    lunit = Iso fwd bwd
      where
      fwd (_, r) = r
      bwd r = ((), r)
    runit = Iso fwd bwd
      where
      fwd (l, _) = l
      bwd l = (l, ())
