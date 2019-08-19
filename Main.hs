{-# LANGUAGE
    PolyKinds
  , MultiParamTypeClasses
  , DataKinds
  , FunctionalDependencies
  , GADTs
  , RankNTypes
  , TypeOperators
  , TypeFamilies
  , DuplicateRecordFields
  , NamedFieldPuns
  , NoImplicitPrelude
#-}

module Main where

import Prelude (Int, Maybe, Num(..))

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

data Functor (m :: Hom mi pi) (p :: Hom pi po) (q :: Hom qi qo) (f :: pi -> qi)
  where
  Functor ::
    { source :: Category p m
    , target :: Category q m
    , map :: forall a b. p a b `m` q (f a) (f b)
    } ->
    Functor m p q f

data Monoidal (m :: Hom mi mo) (i :: mi) (t :: mi -> mi -> mi)
  where
  Monoidal ::
    { basis :: Category m n
    , assoc :: forall a b c. Iso m ((a `t` b) `t` c) (a `t` (b `t` c))
    , lunit :: forall l. Iso m (i `t` l) l
    , runit :: forall r. Iso m (r `t` i) r
    } ->
    Monoidal m i t

data Monoid (p :: Hom pi po) (i :: pi) (t :: pi -> pi -> pi) (o :: pi)
  where
  Monoid ::
    { basis :: Monoidal p i t
    , unit :: i `p` o
    , append :: (o `p` o) -> o
    } ->
    Monoid p i t o

data Category (m :: Hom mi pi) (p :: Hom pi po)
  where
  Category ::
    { basis :: Monoidal m i t
    , identity :: forall a. i `m` p a a
    , compose :: forall x y z. (p y z `t` p x y) `m` p x z
    } ->
    Category m p

type SmallCategory = Category (->)
type SmallFunctor = Functor (->)
type SmallEndoFunctor p f = SmallFunctor p p f

hask :: SmallCategory (->)
hask = Category { basis, identity, compose }
  where
  identity _ x = x
  compose (f, g) x = f (g x)
  basis = Monoidal { basis = hask, assoc, lunit, runit }
    where
    assoc = Iso { fwd, bwd }
      where
      fwd ((a, b), c) = (a, (b, c))
      bwd (a, (b, c)) = ((a, b), c)
    lunit = Iso { fwd, bwd }
      where
      fwd (_, r) = r
      bwd r = ((), r)
    runit = Iso { fwd, bwd }
      where
      fwd (l, _) = l
      bwd l = (l, ())

list :: SmallEndoFunctor (->) []
list = Functor { source = hask, target = hask, map }
  where
  map f [] = []
  map f (x : xs) = f x : map f xs

test :: [Int]
test = map list (* 2) [1, 2, 3]
