{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module CLI where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Arrow
import System.Console.Haskeline
import Data.List (intercalate, isPrefixOf)
import qualified Data.UUID as UUID
import qualified System.UUID.V4 as UUID
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.PrettyPrint as PP
import qualified System.IO.Temp as Temp
import System.Process (system, rawSystem)
import System.IO (hPutStrLn, hFlush, hClose)
import System.Posix.Files (getFileStatus, modificationTime)
import System.Directory (doesFileExist)

import Solver
import qualified Javascript as JS

data CLI

data Definition = Definition {
    defCode :: String,
    defDeps :: [String]
}
    deriving (Read,Show)

data Database = Database {
    dbRules       :: Map.Map (PredName CLI) [Rule CLI],
    dbDefns       :: Map.Map (DefID CLI) Definition,
    dbAssumptions :: [Prop CLI],
    dbAssertions  :: [Prop CLI]
}
    deriving (Read,Show)

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
        deriving (Eq,Ord,Read,Show)
    newtype Effect CLI a = Effect { runEffect :: Reader Database a }
    newtype PredName CLI = PredName String
        deriving (Eq,Ord,Read,Show)

    findRules name = maybe [] id <$> Effect (asks (Map.lookup name . dbRules))


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
showEnv db = PP.vcat 
    [ PP.vcat (map showProp (dbAssumptions db))
    , PP.text "----------"
    , PP.vcat (map showProp (dbAssertions db))
    ]

type Parser = P.Parsec String () 

prop :: Parser (Prop CLI)
prop = liftA2 (:@) (PredName <$> P.many1 P.alphaNum) (P.many (space *> object))

object :: Parser (Object CLI)
object = Var <$> P.many1 P.alphaNum


space :: Parser ()
space = P.space *> P.spaces

defineLocal :: String -> String -> Shell ()
defineLocal name code = do
    case makeDefn code of
        Left err -> liftIO . putStrLn $ "Parse error: " ++ err
        Right defn -> do
            addDefn name defn
            liftIO . putStrLn $ "defined " ++ name

defineEditor :: String -> Shell ()
defineEditor name = do
    db <- lift get
    let shEnv = unlines . map ("// " ++) . lines . PP.render $ showEnv db
    let prefix = shEnv ++ "// defining: " ++ name
    let go pfx code = do
            mcontents <- liftIO $ editor (prefix ++ "\n// " ++ pfx) "//////////" code
            case mcontents of
                Just contents -> case makeDefn contents of
                    Left err -> go ("Parse error: " ++ err) contents
                    Right defn -> do
                        addDefn name defn
                        liftIO . putStrLn $ "defined " ++ name
                Nothing -> liftIO . putStrLn $ "cancelled"
    go "" ""
    

cmd :: Parser (Shell ())
cmd = P.choice $ map P.try [
    define <$> (P.string "define" *> space *> P.many1 P.alphaNum),
    defineInline <$> (P.many1 P.alphaNum <* P.spaces <* P.char '=' <* P.spaces) <*> P.many (P.satisfy (const True)),
    eval <$> ((P.string "eval" <|> P.string "!") *> P.many (P.satisfy (const True))),
    env <$> (P.string "env" *> pure ()),
    rules <$> (P.string "rules" *> pure ()),
    assume <$> (P.string "assume" *> space *> prop),
    assert <$> (P.string "assert" *> space *> prop),
    clear <$> (P.string "clear" *> pure ())
    ]

    where
    define = defineEditor

    defineInline = defineLocal

    env () = do
        db <- lift get
        liftIO . putStrLn . PP.render . showEnv $ db

    rules () = do
        db <- lift get
        liftIO . putStrLn . PP.render . PP.vcat . map showRule . concat . Map.elems . dbRules $ db

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

    eval str = do
        code <- assemble
        case code of
            Nothing -> return ()
            Just code' -> do
                liftIO $ writeFile "assemble.js" code'
                liftIO $ rawSystem "node" ["-e", "require('./assemble.js'); console.log(" ++ str ++ ");" ]
                return ()

assemble :: Shell (Maybe String)
assemble = do
    props <- lift $ gets dbAssumptions
    ruleProps <- lift $ gets dbAssertions
    defns <- lift $ gets dbDefns
    let newRules = Map.unionsWith (++) $ map (singletonRule . ([] :=>)) ruleProps
    db <- lift get
    let db' = db { dbRules = Map.unionWith (++) newRules (dbRules db) }
    case map fst $ runReader (runEffect (runSolver (mapM_ satisfy props) 1)) db' of
        [] -> do
            liftIO . putStrLn $ "No solution"
            return Nothing
        [sub] -> do
            let defs = [ (k, assemble' defns v) | (k, v) <- Map.toList sub, not ("~" `isPrefixOf` k) ]
            let code = unlines $ map (\(k,code) -> k ++ " = " ++ code ++ ";") defs
            return $ Just code

assemble' :: Map.Map (DefID CLI) Definition -> Object CLI -> String
assemble' defns (Var x) = "\"<<<Variable " ++ x ++ ">>>\""
assemble' defns (defid :% args) = assembleDefn (defns Map.! defid)
    where
    assembleDefn (Definition {..}) = 
        "(function(" ++ intercalate "," defDeps ++ ") { return (" ++ defCode ++ ") })("
            ++ intercalate "," (map (assemble' defns) args) ++ ")"


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

load :: Shell ()
load = do
    ex <- liftIO $ doesFileExist "tigress.db"
    when ex $ do
        db <- fmap read . liftIO $ readFile "tigress.db"
        lift $ put $! db

save :: Shell ()
save = do
    db <- lift get
    liftIO $ writeFile "tigress.db" (show db)

main = do
    execStateT (runInputT defaultSettings (load >> mainShell >> save)) emptyDatabase
    return ()
