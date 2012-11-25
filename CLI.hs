{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, DoAndIfThenElse, EmptyDataDecls, PatternGuards #-}

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
import Data.List (intercalate, isPrefixOf, isInfixOf, tails, partition)
import Data.Maybe (listToMaybe)
import System.Process (system, rawSystem)
import System.IO (hPutStrLn, hFlush, hClose)
import System.Posix.Files (getFileStatus, modificationTime)
import System.Directory (doesFileExist)

import Solver
import qualified Javascript as JS
import qualified Scope

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

data Suite = Suite {
    suiteAssumptions :: [Prop CLI],
    suiteScope :: Scope.Scope String (PredName CLI),
    suiteName :: String,
    suiteDescription :: String
}
    deriving (Read,Show)

data Database = Database {
    dbRuleDocs    :: Map.Map (PredName CLI) RuleDoc,
    dbRules       :: Map.Map (PredName CLI) [Rule CLI],
    dbDefns       :: Map.Map (DefID CLI) Definition,
    dbRuleScope   :: Scope.Scope String (PredName CLI),
    dbSuites      :: [Suite],
    dbAssumptions :: [Prop CLI],
    dbAssertions  :: [Prop CLI]
}
    deriving (Read,Show)

eqDoc :: RuleDoc
eqDoc = RuleDoc {
    rdocName = "eq",
    rdocDescription = "eq\n====\n`eq X Y` asserts that the objects X and Y are equal."
}

emptyDatabase = Database {
    dbRuleDocs = Map.singleton (PredBuiltin PredEq) eqDoc,
    dbRules = singletonRule $ [] :=> PredBuiltin PredEq :@ [ Var "X", Var "X" ],
    dbDefns = Map.empty,
    dbRuleScope = Scope.empty,
    dbSuites = [],
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

mentionedIn :: String -> Prop CLI -> Bool
mentionedIn var (_ :@ objs) = any mentionedIn' objs
    where
    mentionedIn' (Var v) = v == var 
    mentionedIn' (_ :% objs) = any mentionedIn' objs

showObject :: Object CLI -> PP.Doc
showObject (Var v) = PP.text v
showObject (DefID def :% deps) = PP.text (show def) PP.<> PP.brackets (PP.hsep (PP.punctuate PP.comma (map showObject deps)))

showPredName :: Database -> PredName CLI -> PP.Doc
showPredName db (PredBuiltin PredEq) = PP.text "eq"
showPredName db o
    | Just n <- Scope.lookupName o (dbRuleScope db) = PP.text n
    | otherwise                                     = PP.text (show o)

showProp :: Database -> Prop CLI -> PP.Doc
showProp db (n :@ args) = showPredName db n PP.<+> PP.hsep (map showObject args)

showRule :: Database -> Rule CLI -> PP.Doc
showRule db (assns :=> con) = showProp db con PP.<+> PP.text ":-" PP.<+> PP.vcat (map (showProp db) assns)

showEnv :: Database -> PP.Doc
showEnv db = PP.vcat 
    [ PP.vcat (map (showProp db) (dbAssumptions db))
    , PP.text "----------"
    , PP.vcat (map (showProp db) (dbAssertions db))
    ]

type Parser = P.Parsec String () 

predName :: Database -> Parser (PredName CLI)
predName db = P.choice $ map P.try [
    P.string "eq" *> pure (PredBuiltin PredEq),
    P.many1 P.alphaNum >>= \s -> do
        Just name <- return $ Scope.lookupObj s (dbRuleScope db)
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

wordInDoc :: String -> RuleDoc -> Bool
wordInDoc s doc = find (rdocName doc) || find (rdocDescription doc)
    where
    find body = map Char.toLower s `isInfixOf` body

wordInSuite :: String -> Suite -> Bool
wordInSuite s suite = find (suiteName suite) || find (suiteDescription suite)
    where
    find body = map Char.toLower s `isInfixOf` body

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

ppText :: String -> PP.Doc
ppText = PP.vcat . map PP.text . lines

discharge :: (Prop CLI -> Bool) -> Shell ()
discharge match = do
    assumptions <- lift $ gets dbAssumptions
    assertions <- lift $ gets dbAssertions
    let (kill1,keep1) = partition match assumptions
    let (kill2,keep2) = partition match assertions
    
    let newRules = Map.unionsWith (++) . map (singletonRule . (assumptions :=>)) $ kill2
    lift . modify $ \db -> db {
        dbRules = Map.unionWith (++) newRules (dbRules db),
        dbAssumptions = keep1,
        dbAssertions = keep2
    }

clearAbs :: (String -> Bool) -> Shell ()
clearAbs match = do
    scope <- lift $ gets dbRuleScope
    discharge (\(n :@ _) -> (match <$> Scope.lookupName n scope) == Just True)
    lift . modify $ \db -> db {
        dbRuleScope = Scope.fromList . filter (\(n,d) -> not (match n)) . Scope.toList $ dbRuleScope db
    }

searchInterface :: (m -> PP.Doc) -> [m] -> (m -> Shell ()) -> Shell ()
searchInterface showM matches select = do
    forM_ (zip [0..] matches) $ \(i, m) ->
        liftIO . putStrLn . PP.render $ PP.text (show i ++ ")") PP.<+> showM m
    
    input <- getInputLine "? "
    case input >>= maybeRead of
        Just num | num < length matches -> do
            select (matches !! num)
        _ -> liftIO . putStrLn $ "cancelled"

searchDB :: [String] -> Shell ()
searchDB ws = do
    db <- lift get
    let matches = [ (name, doc) | (name,doc) <- Map.toList (dbRuleDocs db), all (`wordInDoc` doc) ws ]
    
    searchInterface (ppText . rdocDescription . snd) matches $ \(name, doc) -> do
        mname <- getInputLineWithInitial "name? " (rdocName doc, "")
        case mname of
            Just lname | not (null lname) -> do
                lift . put $ db {
                    dbRuleScope = Scope.insert lname name (dbRuleScope db)
                }
                liftIO . putStrLn $ "brought " ++ lname ++ " into scope"
            _ -> liftIO . putStrLn $ "cancelled"
                
searchSuite :: [String] -> Shell ()
searchSuite ws = do
    db <- lift get
    let matches = [ suite | suite <- dbSuites db, all (`wordInSuite` suite) ws ]
    
    searchInterface (ppText . suiteDescription) matches $ \suite -> do
        lift . put $ db {
            dbAssumptions = suiteAssumptions suite ++ dbAssumptions db,
            dbRuleScope = suiteScope suite `Scope.union` dbRuleScope db
        }
        liftIO . putStrLn $ "assumed suite " ++ suiteName suite


cmd :: Database -> Parser (Shell ())
cmd db = P.choice $ map P.try [
    define <$> (P.string "define" *> space *> identifier),
    defineInline <$> (identifier <* P.spaces <* P.char '=' <* P.spaces) <*> anything,
    defabs <$> (P.string "defabs" *> space *> absId),
    defsuite <$> (P.string "defsuite" *> space *> absId),
    abs <$> (P.string "abs" *> pure ()),
    search <$> (P.string "search" *> P.spaces *> anything),
    suite <$> (P.string "suite" *> P.spaces *> anything),
    eval <$> ((P.string "eval" <|> P.string "!") *> anything),
    env <$> (P.string "env" *> pure ()),
    assume <$> (P.string "assume" *> space *> prop db),
    assert <$> (P.string "assert" *> space *> prop db),
    clear <$> (P.string "clear" *> P.many (space *> identifier)),
    return (return ())
    ]

    where
    identifier = P.many1 (P.alphaNum <|> P.oneOf("_$"))
    absId = P.many1 (P.alphaNum <|> P.oneOf("-_"))
    anything = P.many (P.satisfy (const True))
    

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
                    dbRuleScope = Scope.insert name pname (dbRuleScope db)
                }
                liftIO . putStrLn $ "defined"

    defsuite name = do
        db <- lift get
        let assumptions = PP.nest 4 . PP.vcat $ map (showProp db) (dbAssumptions db)
        mcontents <- liftIO $ simpleEditor "mkd" (unlines [name, "===", PP.render assumptions])
        case mcontents of
            Nothing -> liftIO . putStrLn $ "cancelled"
            Just contents -> do
                let suite = Suite { 
                        suiteAssumptions = dbAssumptions db,
                        suiteName = name,
                        suiteScope = dbRuleScope db,
                        suiteDescription = contents
                        }
                lift . modify $ \db -> db {
                    dbSuites = suite : dbSuites db
                }
                liftIO . putStrLn $ "defined suite " ++ name
        
    abs () = do
        db <- lift get
        forM_ (Scope.toList (dbRuleScope db)) $ \(k,v) -> do
            case Map.lookup v (dbRuleDocs db) of
                Nothing -> liftIO . putStrLn $ k
                Just doc -> liftIO . putStrLn . PP.render $ PP.text k PP.<+> ppText (rdocDescription doc)

    search = searchDB . words

    suite = searchSuite . words

    env () = do
        db <- lift get
        liftIO . putStrLn . PP.render . showEnv $ db

    assume p = do
        lift . modify $ \db -> db { dbAssumptions = p : dbAssumptions db }

    assert p = do
        lift . modify $ \db -> db { dbAssertions = p : dbAssertions db }

    clear [] = discharge (const True) >> clearAbs (const True)
    clear ids = discharge (\a -> any (`mentionedIn` a) ids)
             >> clearAbs (\n -> any (== n) ids)

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
