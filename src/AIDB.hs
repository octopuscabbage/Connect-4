
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
import Control.Monad.Logger
import Control.Applicative
import AITypes
import Board


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
State
    boardstring String
    value Int 
    Board boardstring
    deriving Show
|]

dbName = "test.db"

-- |Run the initial migrations on the database
-- |Does not need to be called in normal operation, only when new db is made
createDB:: IO ()
createDB = runSqlite dbName $ do runMigration migrateAll 

-- |Inserts a new stateValue into the database
-- |Won't insert if it's already in, also won't 
insertState:: StateWeight -> IO ()
insertState (StateWeight state value) =do
	exists <- containsEntry state
	if not exists then (runSqlite dbName $ (insert $ State stateString value)) >> return ()
		else return ()
	where stateString = minString state

makeEntryIfNone board = do
	exists <- containsEntry board
	if not exists then (runSqlite dbName $ ( insert $ State boardstring 0)) >> return ()
		else return ()
	where boardstring = minString board
		

-- |For some boardstate, get the value
-- |If board isn't present in DB then return nothing
getValue:: Board -> IO (Maybe Int)
getValue board = runSqlite dbName $ do
	boardEntity <- getBy $ Board boardString
	return $ stateValue <$> entityVal <$> boardEntity
	where boardString = minString board

-- |Determine if entry is present in database
containsEntry:: Board -> IO Bool
containsEntry  board =  getValue board  >>= (return . isJust) 

updateValueOfBoards:: [Board] -> Int -> IO ()
updateValueOfBoards boards valueDiff = mapM (\b-> makeEntryIfNone b) boards >> (runSqlite dbName $ (mapM (\b-> updateWhere [StateBoardstring ==. (minString b)] [StateValue +=. valueDiff]) boards) >> return ())

updateWin boards = updateValueOfBoards boards 1
updateLoss boards = updateValueOfBoards boards (-1)

