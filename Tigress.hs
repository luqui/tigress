{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Tigress where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Logic
import Control.Monad.State
import Control.Arrow

class (Eq (DefID c)) => Config c where
    data DefID c :: *

type VarName = String
type PredName = String

infix 7 :@
data Prop c = PredName :@ [Object c]

infix 6 :=>
data Rule c = [Prop c] :=> Prop c

infix 8 :%
data Object c
    = Var VarName
    | DefID c :% [Object c]

data Database c = Database {
    dbRules :: Map.Map PredName [Rule c]
}

emptyDB :: Database c
emptyDB = Database { dbRules = Map.empty }

addRule :: Rule c -> Database c -> Database c
addRule rule@(_ :=> (pred :@ _)) db = 
    db { dbRules = Map.insertWith (++) pred [rule] (dbRules db) }

fromRules :: [Rule c] -> Database c
fromRules = foldr addRule emptyDB

class FreeVars a where freeVars :: a -> Set.Set VarName
instance (FreeVars a) => FreeVars [a] where freeVars = Set.unions . map freeVars
instance (FreeVars a) => FreeVars (Map.Map k a) where freeVars = freeVars . Map.elems
instance FreeVars (Object c) where 
    freeVars (Var v) = Set.singleton v
    freeVars (_ :% args) = freeVars args
instance FreeVars (Prop c) where freeVars (pred :@ objs) = freeVars objs
instance FreeVars (Rule c) where freeVars (hyps :=> con) = freeVars hyps `Set.union` freeVars con

type Substitution c = Map.Map VarName (Object c)

class Subst c a where subst :: Substitution c -> a -> a
instance (Subst c a) => Subst c [a] where subst s = map (subst s)
instance (Subst c a) => Subst c (Map.Map k a) where subst s = Map.map (subst s)
instance Subst c (Object c) where
    subst s (Var v) = maybe (Var v) id (Map.lookup v s)
    subst s (did :% args) = did :% subst s args
instance Subst c (Prop c) where
    subst s (pred :@ args) = pred :@ subst s args
instance Subst c (Rule c) where
    subst s (hyps :=> con) = subst s hyps :=> subst s con

data SolverState c = SolverState {
    ssFresh :: Integer,
    ssSubst :: Substitution c
}

type Solver c = StateT (SolverState c) Logic

runSolver :: Solver c a -> [(Substitution c, a)]
runSolver solver = map (first ssSubst . swap) . observeAll $ runStateT solver state0
    where
    state0 = SolverState { ssFresh = 0, ssSubst = Map.empty }
    swap (x,y) = (y,x)

alloc :: Solver c VarName
alloc = do
    s <- get
    let x = ssFresh s
    put $ s { ssFresh = 1 + x }
    return ("~" ++ show x)

assign :: VarName -> Object c -> Solver c ()
assign v o
    | v `Set.member` freeVars o = mzero
    | otherwise = do
        s <- get
        let s' = Map.insert v o (subst (Map.singleton v o) (ssSubst s))
        put $ s { ssSubst = s' }
      
normalize :: (Subst c a) => a -> Solver c a
normalize x = do
    s <- gets ssSubst
    return $ subst s x
 
unify :: (Config c) => Object c -> Object c -> Solver c ()
unify x y = do
    x' <- normalize x
    y' <- normalize y
    case (x', y') of
        (Var a, Var b)       | a == b  -> return ()
        (Var a, obj)                   -> assign a obj
        (obj, Var b)                   -> assign b obj
        (h :% as, h' :% as') | h == h' -> mapM_ (uncurry unify) =<< zip' as as'
        _                              -> fail "No unification"

satisfy :: (Config c) => Database c -> Prop c -> Solver c ()
satisfy db (pred :@ args) = do
    let (Just rules) = Map.lookup pred (dbRules db)
    msum $ map (applyRule <=< instantiate) rules
    where
    applyRule (hyps :=> (_ :@ args')) = do
        mapM_ (uncurry unify) =<< zip' args args'
        mapM_ (satisfy db) hyps

instantiate :: forall a c. (FreeVars a, Subst c a) => a -> Solver c a
instantiate x = do
    sub <- Map.fromList <$> mapM (\v -> (v,) . Var <$> alloc) (Set.toList (freeVars x))
    return $ subst (sub :: Substitution c) x

zip' :: (Functor m, MonadPlus m) => [a] -> [b] -> m [(a,b)]
zip' [] [] = return []
zip' (x:xs) (y:ys) = ((x,y):) <$> zip' xs ys
zip' _ _ = mzero


{-
appendDB = fromRules [
    [] :=> "append" :@ [ "nil" :% [], Var "Ys", Var "Ys" ],
    [ "append" :@ [ Var "Xs", Var "Ys", Var "Zs" ] ]
       :=> "append" :@ [ "cons" :% [ Var "X", Var "Xs" ], Var "Ys", "cons" :% [ Var "X", Var "Zs" ] ]
    ]

testList :: [Object c] -> Object c
testList = foldr (\x xs -> "cons" :% [ x, xs ]) ("nil" :% [])

appendTest1 = "append" :@ [ testList [ "a" :% [], "b" :% [] ], testList [ "c" :% [], "d" :% [] ], Var "Z" ]
-}
