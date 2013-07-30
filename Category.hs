{-# LANGUAGE PolyKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, OverlappingInstances #-}

module Category where

import Prelude hiding (id, (.), fst, snd)
import qualified Prelude

class Category hom where
    id  :: hom a a
    (.) :: hom b c -> hom a b -> hom a c

instance Category (->) where
    id = Prelude.id
    (.) = (Prelude..)

class (Category hom, Category hom') => GFunctor hom hom' f | f -> hom hom' where
    gfmap :: hom a b -> hom' (f a) (f b)

instance GFunctor (->) (->) []    where gfmap = fmap
instance GFunctor (->) (->) Maybe where gfmap = fmap


class (Category hom) => Product (hom :: k -> k -> *) (prod :: k -> k -> k) where
    fst :: hom (prod a b) a
    snd :: hom (prod a b) b
    (&&&) :: hom a b -> hom a c -> hom a (prod b c)
    (***) :: hom a b -> hom c d -> hom (prod a c) (prod b d)
    f *** g = (f . fst) &&& (g . snd)

first :: (Product hom prod) => hom a b -> hom (prod a c) (prod b c)
first = (*** id)

second :: (Product hom prod) => hom a b -> hom (prod c a) (prod c b)
second = (id ***)



instance Product (->) (,) where
    fst = Prelude.fst
    snd = Prelude.snd
    (f &&& g) x = (f x, g x)
    (f *** g) (x,y) = (f x, g y)


class (Category hom) => Coproduct (hom :: k -> k -> *) (coprod :: k -> k -> k) where
    inl :: hom a (coprod a b)
    inr :: hom b (coprod a b)
    (|||) :: hom a c -> hom b c -> hom (coprod a b) c
    (+++) :: hom a c -> hom b d -> hom (coprod a b) (coprod c d)
    f +++ g = (inl . f) ||| (inr . g)

instance Coproduct (->) Either where
    inl = Left
    inr = Right
    (|||) = either

left :: (Coproduct hom coprod) => hom a b -> hom (coprod a c) (coprod b c)
left = (+++ id)

right :: (Coproduct hom coprod) => hom a b -> hom (coprod c a) (coprod c b)
right = (id +++)
