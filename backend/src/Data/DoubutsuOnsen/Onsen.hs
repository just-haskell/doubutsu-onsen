{-# LANGUAGE DeriveGeneric #-}

module Data.DoubutsuOnsen.Onsen (
  Onsen (..), Status (..),
  ) where

import GHC.Generics (Generic)
import Data.Int (Int16, Int32)
import Data.Time (LocalTime)
import Data.Text (Text)

import Data.DoubutsuOnsen.Doubutsu as Doubutsu
import Data.DoubutsuOnsen.Item as Item


data Onsen =
  Onsen
  { onsenId    :: Int16
  , onsenName  :: Text
  , releasedAt :: LocalTime
  } deriving (Eq, Generic)

data Status =
  Status
  { onsen         :: Onsen
  , onsenLevel    :: Int16
  , missionStatus :: Int16
  , seed          :: Int32
  , updatedAt     :: LocalTime
  , startedAt     :: LocalTime
  , doubutsuSlots :: [Doubutsu.SlotStatus [Doubutsu.Coord]]
  , itemSlots     :: [Item.SlotStatus]
  } deriving (Eq, Generic)
