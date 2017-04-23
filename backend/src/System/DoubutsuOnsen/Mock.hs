{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module System.DoubutsuOnsen.Mock () where

import Control.Monad
import Data.Int (Int16, Int32)
import Data.Time (LocalTime, parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Aeson.Encode.Pretty (encodePretty)
-- import Database.Record (ToSql)
import Database.Relational.Query (relationalQuery)
-- import Database.HDBC (SqlValue)
import Database.HDBC (commit)
import Database.HDBC.Session (withConnectionIO')
import Database.HDBC.Record (runInsert, runQuery)
-- import Database.HDBC.PostgreSQL (Connection)

-- import Database.Relational.Extra.SequenceHDBC (autoPool)
-- import qualified Database.Relational.Extra.Sequence as Sequence

import Data.DoubutsuOnsen.InstancesAeson ()
import Database.DoubutsuOnsen.DataSource (connect)
import Database.DoubutsuOnsen.Entity.Onsen (Onsen (Onsen), insertOnsen)
import qualified Database.DoubutsuOnsen.Entity.Onsen as Onsen
import Database.DoubutsuOnsen.Entity.OnsenStatus (OnsenStatus (OnsenStatus), insertOnsenStatus)
import qualified Database.DoubutsuOnsen.Entity.OnsenStatus as OnsenStatus
import Database.DoubutsuOnsen.Entity.Doubutsu (Doubutsu (Doubutsu), insertDoubutsu)
import Database.DoubutsuOnsen.Entity.DoubutsuCoord (DoubutsuCoord (DoubutsuCoord), insertDoubutsuCoord)
import Database.DoubutsuOnsen.Entity.Item (Item (Item), insertItem)
import Database.DoubutsuOnsen.Entity.Slot (Slot (Slot), insertSlot)
import Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu
  (SlotStatusDoubutsu (SlotStatusDoubutsu), insertSlotStatusDoubutsu)
import Database.DoubutsuOnsen.Entity.SlotStatusItem
  (SlotStatusItem (SlotStatusItem), insertSlotStatusItem)
import Database.DoubutsuOnsen.InstanceHDBC ()
import Database.DoubutsuOnsen.Model.Status
  (relSlotDoubutsu, relSlotItem, relOnsenStatus, )
import System.DoubutsuOnsen.Query (queryOnsenStatus)


testGameId :: Int32
testGameId = 1

testOnsenId :: Int16
testOnsenId = 1

parseLocalTime :: String -> LocalTime
parseLocalTime s =
  maybe (error $ "fail to parse time: " ++ s) id
  $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" s

testOnsen :: Onsen
testOnsen =
  Onsen
  { Onsen.id = testOnsenId
  , Onsen.onsenName = "はすけ湯"
  , Onsen.releasedAt = parseLocalTime "2017-04-23 16:05:46"
  }

testOnsenStatus :: OnsenStatus
testOnsenStatus =
  OnsenStatus
  { OnsenStatus.gameId = testGameId
  , OnsenStatus.onsenId = testOnsenId
  , OnsenStatus.onsenLevel = 1
  , OnsenStatus.missionStatus = 0
  , OnsenStatus.seed = 37
  , OnsenStatus.updatedAt = parseLocalTime "2017-04-23 16:05:59"
  , OnsenStatus.startedAt = parseLocalTime "2016-12-04 11:47:43"
  -- , updatedAt = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
  }

testDoubutsuList :: [Doubutsu]
testDoubutsuList =
  [ Doubutsu 1 "さる" (parseLocalTime "2017-04-23 16:05:47")
  , Doubutsu 2 "くま" (parseLocalTime "2017-04-23 16:05:48")
  ]

testDoubutsuCoordList :: [DoubutsuCoord]
testDoubutsuCoordList =
  [ DoubutsuCoord 1 1 0 0
  , DoubutsuCoord 2 1 0 0
  , DoubutsuCoord 2 2 1 0
  , DoubutsuCoord 2 3 0 1
  , DoubutsuCoord 2 4 1 1
  ]

testItemList :: [Item]
testItemList =
  [ Item 1 "ごま" 0
  , Item 2 "くるみ" 1
  ]

mkSlot :: Int16 -> Int16 -> Int16 -> Int16 -> Slot
mkSlot = Slot testOnsenId

testSlotList :: [Slot]
testSlotList =
  [ mkSlot 1 1 1 0
  , mkSlot 2 2 1 0
  , mkSlot 3 3 1 0
  , mkSlot 4 1 2 0
  , mkSlot 5 2 2 0
  , mkSlot 6 3 2 0
  , mkSlot 7 1 3 0
  , mkSlot 8 2 3 0
  , mkSlot 9 3 3 0

  , mkSlot 10 0 1 1
  , mkSlot 11 0 2 1
  , mkSlot 12 1 0 1
  , mkSlot 13 2 0 1
  ]

{-
y +-+-+-+-+
  | |0|0|0|
  +-+-+-+-+
  |1|0|0|0|
  +-+-+-+-+
  |1|0|0|0|
  +-+-+-+-+
  | |1|1| |
  +-+-+-+-+
          x
 -}

mkSlotStatusDoubutsu :: Int16 -> Maybe Int16 -> SlotStatusDoubutsu
mkSlotStatusDoubutsu =
  SlotStatusDoubutsu testGameId testOnsenId

testSlotStatusDoubutsuList :: [SlotStatusDoubutsu]
testSlotStatusDoubutsuList =
  [ mkSlotStatusDoubutsu 1 (Just 1)
  , mkSlotStatusDoubutsu 5 (Just 2)
  ]

mkSlotStatusItem :: Int16 -> Maybe Int16 -> Int16 -> SlotStatusItem
mkSlotStatusItem =
  SlotStatusItem testGameId testOnsenId

testSlotStatusItemList :: [SlotStatusItem]
testSlotStatusItemList =
  [ mkSlotStatusItem 10 (Just 2) 1
  , mkSlotStatusItem 13 (Just 1) 2
  ]

-- onsenSeqPool :: IO [Sequence.Number Onsen Int16]
-- onsenSeqPool = autoPool connect 1 onsen

-- doubutsuSeqPool :: IO [Sequence.Number Doubutsu Int16]
-- doubutsuSeqPool = autoPool connect 1 doubutsu

-- itemSeqPool :: IO [Sequence.Number Item Int16]
-- itemSeqPool = autoPool connect 1 item

_initializeMockEnv :: IO ()
_initializeMockEnv = withConnectionIO' connect $ \conn -> do
  void $ runInsert conn insertOnsen testOnsen
  void $ runInsert conn insertOnsenStatus testOnsenStatus
  let mapInsert i = mapM_ (runInsert conn i)
  mapInsert  insertDoubutsu            testDoubutsuList
  mapInsert  insertDoubutsuCoord       testDoubutsuCoordList
  mapInsert  insertItem                testItemList
  mapInsert  insertSlot                testSlotList
  mapInsert  insertSlotStatusDoubutsu  testSlotStatusDoubutsuList
  mapInsert  insertSlotStatusItem      testSlotStatusItemList
  commit conn

_printOnsenStatus :: IO ()
_printOnsenStatus = withConnectionIO' connect $ \conn ->
  B8.putStrLn . encodePretty =<< runQuery conn (relationalQuery relOnsenStatus) (testGameId, testOnsenId)

_printSlotStatusDoubutsu :: IO ()
_printSlotStatusDoubutsu = withConnectionIO' connect $ \conn ->
  B8.putStrLn . encodePretty =<< runQuery conn (relationalQuery relSlotDoubutsu) (testGameId, testOnsenId)

_printSlotStatusItem :: IO ()
_printSlotStatusItem = withConnectionIO' connect $ \conn ->
  B8.putStrLn . encodePretty =<< runQuery conn (relationalQuery relSlotItem) (testGameId, testOnsenId)

_printOnsenStatusAll :: IO ()
_printOnsenStatusAll =
  B8.putStrLn . encodePretty =<< queryOnsenStatus testGameId testOnsenId
