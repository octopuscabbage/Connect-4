
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
import AITypes


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
State
    boardstring String
    value Int 
    Board boardstring
    deriving Show
|]

dbName:: Text
dbName = "test.db"

-- |Run the initial migrations on the database
-- |Does not need to be called in normal operation, only when new db is made
createDB:: IO ()
createDB = runSqlite dbName $ do runMigration migrateAll 

-- |Inserts a new stateValue into the database
-- |Won't insert if it's already in, also won't 
insertState:: StateWeight -> IO ()
insertState (StateWeight state value) = do
	exists <- containsEntry stateString
	if not exists then (runSqlite dbName $ (insert $ State stateString value)) >> return ()
		else return ()
	where stateString = show state
		

-- |For some boardstate, get the value
-- |If board isn't present in DB then return nothing
getValue:: String -> IO (Maybe Int)
getValue board = runSqlite dbName $ do
	boardEntity <- getBy $ Board board
	return $ stateValue <$> entityVal <$> boardEntity

-- |Determine if entry is present in database
containsEntry:: String -> IO Bool
containsEntry  board =  getValue board  >>= (return . isJust) 

-- |TODO: Add update on win function
-- |TODO: Add update on loss function
