{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module CLI where

import Solver
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import System.Console.Haskeline
import qualified Data.UUID as UUID
import qualified System.UUID.V4 as UUID
import qualified Data.Map as Map
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified System.IO.Temp as Temp
import System.Process (system)
import System.IO (hPutStrLn, hFlush, hClose)
import System.Posix.Files (getFileStatus, modificationTime)

data CLI

data Definition = Definition {
    defCode :: String
}

data Database = Database {
    dbRules :: Map.Map (PredName CLI) [Rule CLI],
    dbDefns :: Map.Map (DefID CLI) Definition,
    dbEnv   :: [Prop CLI]
}

emptyDatabase = Database {
    dbRules = Map.empty,
    dbDefns = Map.empty,
    dbEnv = Map.empty
}

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
    let prop = PredName "eq" [Var localname, uuid :% []]
    lift . modify $ \db -> db { dbDefns = Map.insert uuid defn (dbDefns db)
                              , dbEnv = Map.insert localName uuid (dbEnv db) }

indent :: String -> String -> String
indent pre = unlines . map (pre ++) . lines

type Parser = P.Parsec String () 

prop :: DB -> Parser (Prop CLI)
prop db = liftA2 (:@) (P.many1 P.alphaNum) (P.many (object db))

object :: DB -> Parser (Oject CLI)
object db = wiffle <$> P.many1 P.alphaNum
    where
    wiffle ident 
        | Just x <- Map.lookup ident (dbEnv db) 

space :: Parser ()
space = P.space *> P.spaces

cmd :: Parser (Shell ())
cmd = P.choice [
    define <$> ((P.string "define" <|> P.string "def") *> space *> P.many1 P.alphaNum),
    env <$> (P.string "env" *> pure ())
    ]

    where
    define name = do
        mcontents <- liftIO $ editor ("// " ++ name) "//////////" ""
        case mcontents of
            Just contents -> do
                addDefn name (Definition { defCode = contents })
                liftIO . putStrLn $ "defined " ++ name
            Nothing ->
                liftIO . putStrLn $ "cancelled"
    env () = do
        db <- lift get
        liftIO . forM_ (Map.toList (dbEnv db)) $ \(k,v) -> do
            putStrLn k
            putStrLn . indent "    " . defCode $ dbDefns db Map.! v

mainShell :: Shell ()
mainShell = do
    inp <- getInputLine "> "
    case inp of
        Nothing -> return ()
        Just line -> do
            case P.runParser cmd () "<input>" line of
                Left err -> liftIO $ print err
                Right m -> m
            mainShell


main = execStateT (runInputT defaultSettings mainShell) emptyDatabase
