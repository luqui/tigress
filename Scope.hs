module Scope 
    ( Scope, empty, toList, insert, lookupObj, lookupName, delete )
where

import qualified Data.Map as Map

data Scope name obj = Scope {
    nameToObj :: Map.Map name obj,
    objToName :: Map.Map obj name
}
    deriving (Read,Show)

empty :: Scope name obj
empty = Scope { nameToObj = Map.empty, objToName = Map.empty }

toList :: Scope name obj -> [(name, obj)]
toList = Map.toList . nameToObj

insert :: (Ord name, Ord obj) => name -> obj -> Scope name obj -> Scope name obj
insert name obj scope = Scope {
    nameToObj = Map.insert name obj (nameToObj scope),
    objToName = Map.insert obj name (objToName scope)
}

lookupObj :: (Ord name) => name -> Scope name obj -> Maybe obj
lookupObj name scope = Map.lookup name (nameToObj scope)

lookupName :: (Ord obj) => obj -> Scope name obj -> Maybe name
lookupName obj scope = Map.lookup obj (objToName scope)

delete :: (Ord name, Ord obj) => name -> Scope name obj -> Scope name obj
delete name scope = Scope {
    nameToObj = Map.delete name (nameToObj scope),
    objToName = maybe id Map.delete (lookupObj name scope) $ objToName scope
}
