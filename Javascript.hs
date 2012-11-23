{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

-- module Javascript where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import qualified Data.Set as Set
import Data.Generics
import Data.Typeable
import Data.Monoid
import Control.Arrow
import Text.Show.Pretty (ppShow)
import Debug.Trace


data Vars = Vars {
    freeVars :: Set.Set String,
    definedVars :: Set.Set String
}
    deriving Show

instance Monoid Vars where
    mempty = Vars Set.empty Set.empty
    mappend (Vars f d) (Vars f' d') = Vars (f `Set.union` f') (d `Set.union` d')

vars :: String -> Either String Vars
vars i = right gvars $ parse i "<input>"

countChildren :: (Data a) => a -> Int
countChildren x | trace ("Traversing a " ++ show (typeOf x)) False = undefined
countChildren x = succ . sum . gmapQ countChildren $ x

traced f x = trace (show (f x)) x

gvars :: (Data a) => a -> Vars
gvars x | trace ("Traversing a " ++ show (typeOf x)) False = undefined
gvars x = case cast x of
    Just n | Just vs <- query n -> vs
    y                           -> mconcat (gmapQ gvars y)

getIdentifier :: JSNode -> String
getIdentifier (NN (JSIdentifier s)) = s
getIdentifier (NT (JSIdentifier s) _ _) = s
getIdentifier x = error $ "Not an identifier: " ++ show x

query :: Node -> Maybe Vars
query x | trace (show x) False = undefined
query (JSFunction _ nameS _ paramsS _ body) = Just $ Vars {
        freeVars = freeVars vbody `Set.difference` Set.fromList (map getIdentifier paramsS),
        definedVars = Set.singleton (getIdentifier nameS)
    }
    where vbody = gvars body
query (JSFunctionExpression _ namesS _ paramsS _ body) = Just $ Vars {
        freeVars = freeVars vbody `Set.difference` Set.fromList (map getIdentifier paramsS),
        definedVars = Set.fromList (map getIdentifier namesS)
    }
    where vbody = gvars body
query (JSVarDecl name init) = Just $ Vars {
        freeVars = freeVars (gvars init),
        definedVars = Set.singleton (getIdentifier name)
    }
query (JSWith _ _ e _ block) = Just $ Vars {  -- ignore free variables in body of with
        freeVars = freeVars ve,
        definedVars = definedVars ve `Set.union` definedVars (gvars block)
    }
    where ve = gvars e 
query (JSIdentifier ident) = Just $ Vars {
        freeVars = Set.singleton ident,
        definedVars = Set.empty
    }
query x = Nothing
