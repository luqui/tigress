{-# LANGUAGE TupleSections #-}

module Tigress where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Logic
import Control.Monad.State
import Control.Arrow

class Concrete c where

type VarName = String
type PredName = String
type DefID = String

data Prop = App PredName [Object]

data Rule = [Prop] :=> Prop

data Object
    = Var VarName
    | Cons DefID [Object]
    deriving (Show)

class FreeVars a where freeVars :: a -> Set.Set VarName
instance (FreeVars a) => FreeVars [a] where freeVars = Set.unions . map freeVars
instance (FreeVars a) => FreeVars (Map.Map k a) where freeVars = freeVars . Map.elems
instance FreeVars Object where 
    freeVars (Var v) = Set.singleton v
    freeVars (Cons _ args) = freeVars args
instance FreeVars Prop where freeVars (App pred objs) = freeVars objs
instance FreeVars Rule where freeVars (hyps :=> con) = freeVars hyps `Set.union` freeVars con

class Subst a where subst :: Map.Map VarName Object -> a -> a
instance (Subst a) => Subst [a] where subst s = map (subst s)
instance (Subst a) => Subst (Map.Map k a) where subst s = Map.map (subst s)
instance Subst Object where
    subst s (Var v) = maybe (Var v) id (Map.lookup v s)
    subst s (Cons did args) = Cons did (subst s args)
instance Subst Prop where
    subst s (App pred args) = App pred (subst s args)
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
        (Var a, Var b)           | a == b  -> return ()
        (Var a, obj)                       -> assign a obj
        (Cons h as, Cons h' as') | h == h' -> mapM_ (uncurry unify) =<< zip' as as'
        _                                  -> fail "No unification"






zip' :: (Functor m, MonadPlus m) => [a] -> [b] -> m [(a,b)]
zip' [] [] = return []
zip' (x:xs) (y:ys) = ((x,y):) <$> zip' xs ys
zip' _ _ = mzero
