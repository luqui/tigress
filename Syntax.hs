{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, ExistentialQuantification, TypeFamilies #-}

module Syntax where

import Prelude.Extras
import qualified Bound as B
import Data.Foldable
import Data.Traversable
import Data.Void
import qualified Data.Map as Map
import qualified UnificationSolver as Solver

data Exp a
    = EVar a
    | EApply (Exp a) (Exp a)
    | ELambda (B.Scope () Exp a)
    | ELit Literal
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

infixl 9 %
(%) :: Exp a -> Exp a -> Exp a
(%) = EApply


instance Eq1 Exp
instance Ord1 Exp
instance Show1 Exp

instance Monad Exp where
    return = EVar
    EVar x      >>= f = f x
    EApply e e' >>= f = EApply (e >>= f) (e' >>= f)
    ELambda s   >>= f = ELambda (s B.>>>= f)
    ELit l      >>= _ = ELit l

data Literal
    = LString String
    | LInt    Integer
    deriving (Eq, Ord, Show)



-- A `Theory con n` is a relationship between a set of variables in `n`,
-- constrained over the functor `con`.
data Theory con n = Theory [n] (con n)
    deriving (Eq, Ord, Show)


data Instantiation th v = forall n. (Ord n) => Instantiation (th n) (n -> v)
data Rule th = forall v. (Ord v) => Rule [v] [Instantiation th (Exp v)] (Instantiation th (Exp v))






{-
// Example

theory Constraint {
    Constraint   // A notion of constraint, e.g. a type judgment, a doc snippet
    field        // A function taking any object and returning a trivial "mention" constraint
    empty        // No constraints
    combine      // A binary function combining two constraints into one.  Used internally!
}

theory DocConstraint {
    import Constraint
    field doc          // A function taking a builtin string and returning a documentation constraint
}

theory Product {
    Product : Type -> Type -> Type
    &&& : (a -> b) -> (a -> c) -> (a -> Product b c)
    fst : Product a b -> a
    snd : Product a b -> b
}

model {
    f : forall a b. F a b
    --
    f : forall a b. F b a
}

model {
    f : F a b        -- apply(apply(F,a),b)
    --
    f : a -> (a,b)   -- apply(apply(->, a), apply(apply(",", a), b))
}
-- Can the above model be used with rankN, or do we have to

model {
    f' : (forall a b. F a b -> a -> (a,b))
}

model {
    Constraint[ Constraint=Constraint1, field=field1, empty=empty1, combine=combine1 ]
    Constraint[ Constraint=Constraint2, field=field2, empty=empty2, combine=combine2 ]
    --
    Constraint[ Constraint=(Product Constraint1 Constraint2)
              , field=(field1 &&& field2)
              , empty=(empty1, empty2)
              , combine=(\x y -> (combine1 x y, combine2 x y)) 
              ]
}

// another way to do the above, more generally

theory FAlgebra {
    F : Type -> Type
    cons : F A -> A
}

theory ConstraintF {
    ConstraintF : Type -> Type
}

ConstraintF = fsum [ K Any, K Unit, Pair ]

model {
    Constraint[ Constraint=Constraint, field=field, empty=empty, combine=combine ]
    --
    FAlgebra[ F = ConstraintF
            , A = Constraint
            , cons = fanIn [ field, const empty, uncurry combine ] ]
}

model {
    FAlgebra [ F = ConstraintF, A = A, cons = cons ]
    --
    Constraint [ Constraint=Constraint, field = cons . in 0, empty = cons (in 1 Unit), combine = curry (cons . in 2) ]
}

model {
    Product [ Product = Product, fst = fst, snd = snd, &&& = &&& ]
    FAlgebra [ F = F, A = A, cons = consA ]
    FAlgebra [ F = F, A = B, cons = consB ]
    -- 
    FAlgebra [ F = F, A = Product A B, cons = consA . fmap fst &&& consB . fmap snd ]
}



theory TypeConstraint {
    import DocConstraint
    doc "`Type` is an object representing the type of types"
    doc "`:` is a binary function taking a term and a type and returning a type constraint."
}

theory String {
    import Type   // import Type (Type, :)
    String : Type
    doc "`String` represents a finite sequence of characters"
}

theory StringAppend {
    import String
    doc "`++` is a binary operator that concatenates strings"
}

theory String {
    import String
    ++      // A binary operator to concatenate strings
    string  // A function to construct strings out of builtin-strings
}

theory Escape {
    import String
    escape
}

theory Lambda {
    import Type
    ->     // A type constructor of functions
    fun    // A function to construct a function out of a builtin lambda
    apply  // A function to apply a constructed function to an argument
}


// Simplest model: Types -> ()
//                 Things representable in core calculus -> themselves

model Builtin of Type {
    Type = ()
    :    = ()
}

model Builtin_String of String {
    import model Builtin
    String = ()
}

// StringMonoid unimplementable because of ++
// Escape unimplmentable

model Lambda {
    import model Builtin
    -> = \x y -> ()
    fun = \f -> f
    apply = \f x -> f x
}


// Haskell-String model: Types -> Strings representing haskell types
//                       Terms -> Strings representing haskell terms

A1: "Types are represented by `String`s containing their Haskell encoding"
A2: "Terms are represented by `String`s containing their Haskell encoding"

model HSType of Type {
    import String
    import Lambda    // notions of Type unified?
    --
    Type = string "*"   
    :    = fun \x -> fun \y -> x ++ string " :: " ++ y
}

model HSType_String of String {
    // Couplings are explicit.
    // "import model" indicates coupling.
    // (Still, this creates a potentially unboundedly increasing boundary of abstraction
    // by transitively expanding imports. Is there a more modular way?)
    import model HSType
    --
    String = string "String"   // A1
}

model of StringMonoid {
    import model HSType_String
    --
    ++ = \x y -> string "(" ++ x ++ string " ++ " ++ y ++ string ")"  // A2
    string = \s -> string "\"" ++ escape s ++ string "\""             // A2
}

model of Escape {
    import model HSType_String
    --
    escape = string "(let esc1 '\"' = \"\\\\\\\"\" ; esc1 x = x in concatMap esc1)"  // A2
}

model of Lambda {
    import model HSType
    --
    -> = fun \x -> fun \y -> string "(" ++ x ++ string " -> " ++ y ++ string ")"  // A1
    fun = fun \f -> string "(\\x -> " ++ apply f (string "x") ++ ")"              // A2
}



-}

