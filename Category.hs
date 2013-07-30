{-# LANGUAGE PolyKinds, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, OverlappingInstances #-}

module Category where

import Prelude hiding (id, (.))
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
