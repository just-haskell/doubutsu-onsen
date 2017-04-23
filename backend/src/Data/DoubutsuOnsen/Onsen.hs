{-# LANGUAGE DeriveGeneric #-}

module Data.DoubutsuOnsen.Onsen (
  Onsen (..), Status (..),
  ) where

import GHC.Generics (Generic)
import Data.Int (Int16, Int32)
import Data.Time (LocalTime)
import Data.Text (Text)


data Onsen =
  Onsen
  { onsenId    :: Int16
  , onsenName  :: Text
  , releasedAt :: LocalTime
  } deriving (Eq, Generic)

data Status a b =
  Status
  { onsen         :: Onsen
  , onsenLevel    :: Int16
  , missionStatus :: Int16
  , seed          :: Int32
  , updatedAt     :: LocalTime
  , startedAt     :: LocalTime
  , doubutsuSlots :: a
  , itemSlots     :: b
  } deriving (Eq, Generic)
