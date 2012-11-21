{-# LANGUAGE TupleSections #-}

module Tigress where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Logic
import Control.Monad.State
import Control.Arrow

class Concrete c where

type VarName = String
type PredName = String
type DefID = String

infix 7 :@
data Prop = PredName :@ [Object]
    deriving (Show)

infix 6 :=>
data Rule = [Prop] :=> Prop
    deriving (Show)

infix 8 :%
data Object
    = Var VarName
    | DefID :% [Object]
    deriving (Show)

data Database = Database {
    dbRules :: Map.Map PredName [Rule]
}
    deriving (Show)

emptyDB :: Database
emptyDB = Database { dbRules = Map.empty }

addRule :: Rule -> Database -> Database
addRule rule@(_ :=> (pred :@ _)) db = 
    db { dbRules = Map.insertWith (++) pred [rule] (dbRules db) }

fromRules :: [Rule] -> Database
fromRules = foldr addRule emptyDB

class FreeVars a where freeVars :: a -> Set.Set VarName
instance (FreeVars a) => FreeVars [a] where freeVars = Set.unions . map freeVars
instance (FreeVars a) => FreeVars (Map.Map k a) where freeVars = freeVars . Map.elems
instance FreeVars Object where 
    freeVars (Var v) = Set.singleton v
    freeVars (_ :% args) = freeVars args
instance FreeVars Prop where freeVars (pred :@ objs) = freeVars objs
instance FreeVars Rule where freeVars (hyps :=> con) = freeVars hyps `Set.union` freeVars con

class Subst a where subst :: Map.Map VarName Object -> a -> a
instance (Subst a) => Subst [a] where subst s = map (subst s)
instance (Subst a) => Subst (Map.Map k a) where subst s = Map.map (subst s)
instance Subst Object where
    subst s (Var v) = maybe (Var v) id (Map.lookup v s)
    subst s (did :% args) = did :% subst s args
instance Subst Prop where
    subst s (pred :@ args) = pred :@ subst s args
instance Subst Rule where
    subst s (hyps :=> con) = subst s hyps :=> subst s con

data SolverState = SolverState {
    ssFresh :: Integer,
    ssSubst :: Map.Map VarName Object
}

type Solver = StateT SolverState Logic

runSolver :: Solver a -> [(Map.Map VarName Object, a)]
runSolver solver = map (first ssSubst . swap) . observeAll $ runStateT solver state0
    where
    state0 = SolverState { ssFresh = 0, ssSubst = Map.empty }
    swap (x,y) = (y,x)

alloc :: Solver VarName
alloc = do
    s <- get
    let x = ssFresh s
    put $ s { ssFresh = 1 + x }
    return ("~" ++ show x)

assign :: VarName -> Object -> Solver ()
assign v o
    | v `Set.member` freeVars o = mzero
    | otherwise = do
        s <- get
        let s' = Map.insert v o (subst (Map.singleton v o) (ssSubst s))
        put $ s { ssSubst = s' }
      
normalize :: (Subst a) => a -> Solver a
normalize x = do
    s <- gets ssSubst
    return $ subst s x
 
unify :: Object -> Object -> Solver ()
unify x y = do
    x' <- normalize x
    y' <- normalize y
    case (x', y') of
        (Var a, Var b)       | a == b  -> return ()
        (Var a, obj)                   -> assign a obj
        (obj, Var b)                   -> assign b obj
        (h :% as, h' :% as') | h == h' -> mapM_ (uncurry unify) =<< zip' as as'
        _                              -> fail "No unification"

satisfy :: Database -> Prop -> Solver ()
satisfy db (pred :@ args) = do
    let (Just rules) = Map.lookup pred (dbRules db)
    msum $ map (applyRule <=< instantiate) rules
    where
    applyRule (hyps :=> (_ :@ args')) = do
        mapM_ (uncurry unify) =<< zip' args args'
        mapM_ (satisfy db) hyps

instantiate :: (FreeVars a, Subst a) => a -> Solver a
instantiate x = do
    sub <- Map.fromList <$> mapM (\v -> (v,) . Var <$> alloc) (Set.toList (freeVars x))
    return $ subst sub x

zip' :: (Functor m, MonadPlus m) => [a] -> [b] -> m [(a,b)]
zip' [] [] = return []
zip' (x:xs) (y:ys) = ((x,y):) <$> zip' xs ys
zip' _ _ = mzero


appendDB = fromRules [
    [] :=> "append" :@ [ "nil" :% [], Var "Ys", Var "Ys" ],
    [ "append" :@ [ Var "Xs", Var "Ys", Var "Zs" ] ]
       :=> "append" :@ [ "cons" :% [ Var "X", Var "Xs" ], Var "Ys", "cons" :% [ Var "X", Var "Zs" ] ]
    ]

testList :: [Object] -> Object
testList = foldr (\x xs -> "cons" :% [ x, xs ]) ("nil" :% [])

appendTest1 = "append" :@ [ testList [ "a" :% [], "b" :% [] ], testList [ "c" :% [], "d" :% [] ], Var "Z" ]
