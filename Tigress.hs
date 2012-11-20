module Tigress where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Logic
import Control.Monad.State

class Concrete c where

type VarName = String
type PredName = String

data Prop = App PredName [Object]

data Rule = [Prop] :=> Prop

data Object
    = Var VarName

data Definition c = Definition {
    defnDom :: Set.Set VarName,
    defnCod :: Set.Set VarName,
    defnMap :: Map.Map VarName c -> Map.Map VarName c
}

data Binding c = Binding Rule (Definition c)

class FreeVars a where freeVars :: a -> Set.Set VarName
instance (FreeVars a) => FreeVars [a] where freeVars = Set.unions . map freeVars
instance FreeVars Object where freeVars (Var v) = Set.singleton v
instance FreeVars Prop where freeVars (App pred objs) = freeVars objs
instance FreeVars Rule where freeVars (hyps :=> con) = freeVars hyps `Set.union` freeVars con

checkBinding :: Binding c -> Bool
checkBinding (Binding (hyps :=> con) defn) = (defnDom defn `Set.isSubsetOf` freeVars hyps) && (freeVars con `Set.isSubsetOf` defnCod defn)

data DB c = DB {
    dbBindings :: Map.Map PredName [Binding c]
}

type Solver = StateT Integer Logic

alloc :: Solver VarName
alloc = do
    x <- get
    put $! x+1
    return ("~" ++ show x)

try :: (MonadPlus m) => Maybe a -> m a
try = maybe mzero return





-- bar x y   { x=..; y=.. }
-- bar A Y => foo A z  { z=.. }

-- foo P Q
--  bar P Y
--   

-- list a = nil | cons a (list a)   ;; defines nil, cons, case
-- xs == ys = ... Nil ... Cons ...
-- reverse xs = ... Nil ... Cons ...
-- isPalin xs = xs == reverse xs
-- 
-- (list nil cons case)  ;; relate nil, cons, case as an instance of a list abstraction
--                       ;; (perhaps (list :nil nil :cons cons :case case))
-- 
-- (=> (list nil cons case) (eq ==))  ;; for any list, there is an instance of an == abstraction
-- 
-- (=> (list nil cons case) (reverse reverse)) ;; for any list, there is an instance of a reverse abstraction
-- 
-- (=> (eq ==) (reverse reverse) (isPalin isPalin)) ;; for any eq and reverse instances, there is an isPalin instance
-- 
-- But how do we know that eq and reverse are related at the same point.  There is no way for the solver to
-- connect those, so it might pick an eq on int and a reverse on Map, and end up with nonsense.
-- 
-- Of course, if types are in the game...
-- 
-- (forall (A) (list listA A nil cons case))   ;; list is a polymorphic instance of a list abstraction
-- 
-- (=> (list listA A nil cons case) (eq a ==) (eq list ==*)) ;; if list is an instance of list, and == is an instance of eq, then list is also an instance of eq
-- 
-- (=> (list listA A nil cons case) (reverse listA reverse))
-- 
-- (=> (eq A ==) (reverse A reverse) (isPalin A isPalin))

