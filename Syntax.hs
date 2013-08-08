module Syntax where

import Prelude hiding (lex)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import Control.Applicative

type Parser = P.Parsec String ()

lex :: P.TokenParser ()
lex = P.makeTokenParser $ P.LanguageDef {
    P.commentStart   = "/*",
    P.commentEnd     = "/*",
    P.commentLine    = "//",
    P.nestedComments = True,  -- just to be different (read: better)
    P.identStart     = P.letter <|> P.char '_',
    P.identLetter    = P.alphaNum <|> P.char '_',
    P.opStart        = empty,
    P.opLetter       = empty,
    P.reservedNames  = [],
    P.reservedOpNames = [],
    P.caseSensitive  = True
}


data Decl 
    = Assume TheoryId (Map.Map ParamID Obj)
    | Theory TheoryId [ParamID] TheoryDefn
    | Model TheoryId (Map.Map ParamID Obj)
    | Let ObjId Obj

data Obj
    = ObjId
    | Code (Set.Set ObjId) String

{-
// Example

theory Type {
    Type    // An object representing the type of types
    :       // A binary relation 
    // a basic type theory
    // If X is a `Type`, then x `:` X indicates that X is a term of that type. 
}


theory String {
    import Type
    String 
}

theory StringMonoid {
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

