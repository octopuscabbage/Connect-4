
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module AIDB where
import Database.Persist
import Database.Persist.Sqlite 
import Database.Persist.TH 
import Data.Maybe
import Data.Text
import Control.Monad.IO.Class
import Control.Applicative


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
State
    boardstring String
    value Int 
    Board boardstring
    deriving Show
|]

dbName::Text
dbName = "test.db"


createDB:: IO ()
createDB = runSqlite dbName $ do runMigration migrateAll 

insertState state value = runSqlite dbName $ (insert $ State state value)

getValue:: String -> IO (Maybe Int)
getValue board = runSqlite dbName $ do
	boardEntity <- getBy $ Board board
	return $ stateValue <$> entityVal <$> boardEntity

