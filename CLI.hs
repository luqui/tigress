{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module CLI where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Arrow
import System.Console.Haskeline
import Data.List (intercalate)
import qualified Data.UUID as UUID
import qualified System.UUID.V4 as UUID
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.PrettyPrint as PP
import qualified System.IO.Temp as Temp
import System.Process (system)
import System.IO (hPutStrLn, hFlush, hClose)
import System.Posix.Files (getFileStatus, modificationTime)

import Solver
import qualified Javascript as JS

data CLI

data Definition = Definition {
    defCode :: String,
    defDeps :: [String]
}

data Database = Database {
    dbRules       :: Map.Map (PredName CLI) [Rule CLI],
    dbDefns       :: Map.Map (DefID CLI) Definition,
    dbAssumptions :: [Prop CLI],
    dbAssertions  :: [Prop CLI]
}

emptyDatabase = Database {
    dbRules = singletonRule $ [] :=> PredName "eq" :@ [ Var "X", Var "X" ],
    dbDefns = Map.empty,
    dbAssumptions = [],
    dbAssertions = []
}

singletonRule :: Rule CLI -> Map.Map (PredName CLI) [Rule CLI]
singletonRule r@(_ :=> p :@ _) = Map.singleton p [r]

instance Functor (Effect CLI) where
    fmap f (Effect x) = Effect (fmap f x)
instance Applicative (Effect CLI) where
    pure = Effect . pure
    Effect f <*> Effect x = Effect (f <*> x)
instance Monad (Effect CLI) where
    return = pure
    Effect m >>= f = Effect $ m >>= runEffect . f

instance Config CLI where
    newtype DefID CLI = DefID UUID.UUID
        deriving (Eq, Ord)
    newtype Effect CLI a = Effect { runEffect :: State Database a }
    newtype PredName CLI = PredName String
        deriving (Eq,Ord)

    findRules name = maybe [] id <$> Effect (gets (Map.lookup name . dbRules))


type Shell = InputT (StateT Database IO)

editor :: String -> String -> String -> IO (Maybe String)
editor env delim str = Temp.withSystemTempDirectory "tigress" $ \dir -> do
    Temp.withTempFile dir "edit.js" $ \path handle -> do
        hPutStrLn handle env
        hPutStrLn handle delim
        hPutStrLn handle str
        hClose handle

        stat <- getFileStatus path
        system $ "vim + " ++ path
        stat' <- getFileStatus path
        if modificationTime stat' > modificationTime stat then do
            cts <- readFile path
            return . Just . unlines . safeTail . dropWhile (/= delim) . lines $ cts
        else 
            return Nothing

safeTail [] = []
safeTail (x:xs) = xs

addDefn :: String -> Definition -> Shell ()
addDefn localName defn = do
    uuid <- DefID <$> liftIO UUID.uuid
    let prop = PredName "eq" :@ [Var localName, uuid :% map Var (defDeps defn)]
    lift . modify $ \db -> db { dbDefns = Map.insert uuid defn (dbDefns db)
                              , dbAssumptions = prop : dbAssumptions db }

makeDefn :: String -> Either String Definition
makeDefn code = right (\v -> Definition { defCode = code, defDeps = Set.toList (JS.freeVars v)})
                (JS.vars code)

indent :: String -> String -> String
indent pre = unlines . map (pre ++) . lines

showObject :: Object CLI -> PP.Doc
showObject (Var v) = PP.text v
showObject (DefID def :% deps) = PP.text (show def) PP.<> PP.brackets (PP.hsep (PP.punctuate PP.comma (map showObject deps)))

showProp :: Prop CLI -> PP.Doc
showProp (PredName n :@ args) = PP.text n PP.<+> PP.hsep (map showObject args)

showRule :: Rule CLI -> PP.Doc
showRule (assns :=> con) = showProp con PP.<+> PP.text ":-" PP.<+> PP.vcat (map showProp assns)

showEnv :: Database -> PP.Doc
showEnv db = PP.vcat (map showProp (dbAssumptions db))

type Parser = P.Parsec String () 

prop :: Parser (Prop CLI)
prop = liftA2 (:@) (PredName <$> P.many1 P.alphaNum) (P.many (space *> object))

object :: Parser (Object CLI)
object = Var <$> P.many1 P.alphaNum


space :: Parser ()
space = P.space *> P.spaces

cmd :: Parser (Shell ())
cmd = P.choice [
    define <$> ((P.string "define" <|> P.string "def") *> space *> P.many1 P.alphaNum),
    env <$> (P.string "env" *> pure ()),
    assume <$> (P.string "assume" *> space *> prop),
    assert <$> (P.string "assert" *> space *> prop),
    clear <$> (P.string "clear" *> pure ())
    ]

    where
    define name = do
        mcontents <- liftIO $ editor ("// " ++ name) "//////////" ""
        case mcontents of
            Just contents -> 
                case makeDefn contents of
                    Left err -> liftIO . putStrLn $ err
                    Right defn -> do
                        addDefn name defn
                        liftIO . putStrLn $ "defined " ++ name
            Nothing ->
                liftIO . putStrLn $ "cancelled"

    env () = do
        db <- lift get
        liftIO . putStrLn . PP.render . showEnv $ db

    assume p = do
        lift . modify $ \db -> db { dbAssumptions = p : dbAssumptions db }

    assert p = do
        lift . modify $ \db -> db { dbAssertions = p : dbAssertions db }

    clear () = do
        assumptions <- lift $ gets dbAssumptions
        assertions <- lift $ gets dbAssertions
        let newRules = Map.unionsWith (++) $ map (singletonRule . (assumptions :=>)) assertions
        lift . modify $ \db -> db {
            dbRules = Map.unionWith (++) newRules (dbRules db),
            dbAssumptions = [],
            dbAssertions = []
        }

mainShell :: Shell ()
mainShell = do
    inp <- getInputLine "> "
    case inp of
        Nothing -> return ()
        Just line -> do
            case P.runParser (cmd <* P.eof) () "<input>" line of
                Left err -> liftIO $ print err
                Right m -> m
            mainShell


main = execStateT (runInputT defaultSettings mainShell) emptyDatabase
