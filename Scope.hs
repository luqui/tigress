module Scope 
    ( Scope, empty, toList, fromList, insert, lookupObj, lookupName, union, delete )
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

fromList :: (Ord name, Ord obj) => [(name,obj)] -> Scope name obj
fromList assocs = Scope {
    nameToObj = Map.fromList assocs,
    objToName = Map.fromList (map swap assocs)
    }
    where
    swap (x,y) = (y,x)

insert :: (Ord name, Ord obj) => name -> obj -> Scope name obj -> Scope name obj
insert name obj scope = Scope {
    nameToObj = Map.insert name obj (nameToObj scope),
    objToName = Map.insert obj name (objToName scope)
}

lookupObj :: (Ord name) => name -> Scope name obj -> Maybe obj
lookupObj name scope = Map.lookup name (nameToObj scope)

lookupName :: (Ord obj) => obj -> Scope name obj -> Maybe name
lookupName obj scope = Map.lookup obj (objToName scope)

union :: (Ord name, Ord obj) => Scope name obj -> Scope name obj -> Scope name obj
union s s' = Scope {
    nameToObj = nameToObj s `Map.union` nameToObj s',
    objToName = objToName s `Map.union` objToName s'
}

delete :: (Ord name, Ord obj) => name -> Scope name obj -> Scope name obj
delete name scope = Scope {
    nameToObj = Map.delete name (nameToObj scope),
    objToName = maybe id Map.delete (lookupObj name scope) $ objToName scope
}
