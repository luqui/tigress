{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, DoAndIfThenElse, EmptyDataDecls #-}

module CLI where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Arrow
import System.Console.Haskeline
import qualified Data.UUID as UUID
import qualified System.UUID.V4 as UUID
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.PrettyPrint as PP
import qualified System.IO.Temp as Temp
import qualified Data.Char as Char
import Data.List (intercalate, isPrefixOf, tails)
import Data.Maybe (listToMaybe)
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

data RuleDoc = RuleDoc {
    rdocName :: String,
    rdocDescription :: String
}
    deriving (Read,Show)

data Database = Database {
    dbRuleDocs    :: Map.Map (PredName CLI) RuleDoc,
    dbRules       :: Map.Map (PredName CLI) [Rule CLI],
    dbDefns       :: Map.Map (DefID CLI) Definition,
    dbRuleScope   :: Map.Map String (PredName CLI),
    dbAssumptions :: [Prop CLI],
    dbAssertions  :: [Prop CLI]
}
    deriving (Read,Show)

eqDoc :: RuleDoc
eqDoc = RuleDoc {
    rdocName = "eq",
    rdocDescription = "`eq X Y` asserts that the objects X and Y are equal."
}

emptyDatabase = Database {
    dbRuleDocs = Map.singleton (PredBuiltin PredEq) eqDoc,
    dbRules = singletonRule $ [] :=> PredBuiltin PredEq :@ [ Var "X", Var "X" ],
    dbDefns = Map.empty,
    dbRuleScope = Map.empty,
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

data PredBuiltin
    = PredEq
    deriving (Read,Show,Eq,Ord)

instance Config CLI where
    newtype DefID CLI = DefID UUID.UUID
        deriving (Eq,Ord,Read,Show)
    newtype Effect CLI a = Effect { runEffect :: Reader Database a }
    data PredName CLI = PredUser UUID.UUID | PredBuiltin PredBuiltin
        deriving (Eq,Ord,Read,Show)

    findRules name = maybe [] id <$> Effect (asks (Map.lookup name . dbRules))


type Shell = InputT (StateT Database IO)

simpleEditor :: String -> String -> IO (Maybe String)
simpleEditor ext str = Temp.withSystemTempDirectory "tigress" $ \dir -> do
    Temp.withTempFile dir ("edit." ++ ext) $ \path handle -> do
        hPutStrLn handle str
        hClose handle
        
        stat <- getFileStatus path
        system $ "vim + " ++ path
        stat' <- getFileStatus path
        
        if modificationTime stat' > modificationTime stat 
            then Just <$> readFile path
            else return Nothing
        
editor :: String -> String -> String -> String -> IO (Maybe String)
editor ext env delim str = fmap strip <$> simpleEditor ext (unlines [env, delim, str])
    where
    strip = unlines . safeTail . dropWhile (/= delim) . lines
    
safeTail [] = []
safeTail (x:xs) = xs

addDefn :: String -> Definition -> Shell ()
addDefn localName defn = do
    uuid <- DefID <$> liftIO UUID.uuid
    let prop = PredBuiltin PredEq :@ [Var localName, uuid :% map Var (defDeps defn)]
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
showProp (n :@ args) = PP.text (show n) PP.<+> PP.hsep (map showObject args)

showRule :: Rule CLI -> PP.Doc
showRule (assns :=> con) = showProp con PP.<+> PP.text ":-" PP.<+> PP.vcat (map showProp assns)

showEnv :: Database -> PP.Doc
showEnv db = PP.vcat 
    [ PP.vcat (map showProp (dbAssumptions db))
    , PP.text "----------"
    , PP.vcat (map showProp (dbAssertions db))
    ]

type Parser = P.Parsec String () 

predName :: Database -> Parser (PredName CLI)
predName db = P.choice $ map P.try [
    P.string "eq" *> pure (PredBuiltin PredEq),
    P.many1 P.alphaNum >>= \s -> do
        Just name <- return $ Map.lookup s (dbRuleScope db)
        return name
    ]
    
prop :: Database -> Parser (Prop CLI)
prop db = liftA2 (:@) (predName db) (P.many (space *> object db))

object :: Database -> Parser (Object CLI)
object db = Var <$> P.many1 P.alphaNum


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
            mcontents <- liftIO $ editor "js" (prefix ++ "\n// " ++ pfx) "//////////" code
            case mcontents of
                Just contents -> case makeDefn contents of
                    Left err -> go ("Parse error: " ++ err) contents
                    Right defn -> do
                        addDefn name defn
                        liftIO . putStrLn $ "defined " ++ name
                Nothing -> liftIO . putStrLn $ "cancelled"
    go "" ""

wordIn :: String -> RuleDoc -> Bool
wordIn s doc = find (rdocName doc) || find (rdocDescription doc)
    where
    find body = any (map Char.toLower s `isPrefixOf`) (tails body)
    
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

ppText :: String -> PP.Doc
ppText = PP.vcat . map PP.text . lines

cmd :: Database -> Parser (Shell ())
cmd db = P.choice $ map P.try [
    define <$> (P.string "define" *> space *> P.many1 P.alphaNum),
    defineInline <$> (P.many1 P.alphaNum <* P.spaces <* P.char '=' <* P.spaces) <*> P.many (P.satisfy (const True)),
    defabs <$> (P.string "defabs" *> space *> P.many1 P.alphaNum),
    search <$> (P.string "search" *> space *> P.many (P.satisfy (const True))),
    eval <$> ((P.string "eval" <|> P.string "!") *> P.many (P.satisfy (const True))),
    env <$> (P.string "env" *> pure ()),
    assume <$> (P.string "assume" *> space *> prop db),
    assert <$> (P.string "assert" *> space *> prop db),
    clear <$> (P.string "clear" *> pure ())
    ]

    where
    define = defineEditor

    defineInline = defineLocal

    defabs name = do
        mcontents <- liftIO $ simpleEditor "mkd" (unlines [name, "===="])
        case mcontents of
            Nothing -> liftIO . putStrLn $ "cancelled"
            Just contents -> do
                pname <- PredUser <$> liftIO UUID.uuid
                db <- lift get
                let doc = RuleDoc { rdocName = name, rdocDescription = contents }
                lift . put $ db {
                    dbRuleDocs = Map.insert pname doc (dbRuleDocs db),
                    dbRuleScope = Map.insert name pname (dbRuleScope db)
                }
                liftIO . putStrLn $ "defined"

    search text = do
        db <- lift get
        let ws = words text
        let matches = [ (name, doc) | (name,doc) <- Map.toList (dbRuleDocs db), all (`wordIn` doc) ws ]
        forM_ (zip [0..] matches) $ \(i, (name, doc)) -> 
            liftIO . putStrLn . PP.render $ PP.text (show i ++ ") ") PP.<+> ppText (rdocDescription doc)

        input <- getInputLine "? "
        case input >>= maybeRead of
            Just num | num < length matches -> do
                let match = matches !! num
                mname <- getInputLineWithInitial "name? " (rdocName (snd match), "")
                case mname of
                    Just name | not (null name) -> do
                        lift . put $ db {
                            dbRuleScope = Map.insert name (fst match) (dbRuleScope db)
                        }
                        liftIO . putStrLn $ "brought " ++ name ++ " into scope"
                    _ -> liftIO . putStrLn $ "cancelled"
            _ -> liftIO . putStrLn $ "cancelled"

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
            dbRuleScope = Map.empty,
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
            db <- lift get
            case P.runParser (cmd db <* P.eof) () "<input>" line of
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
