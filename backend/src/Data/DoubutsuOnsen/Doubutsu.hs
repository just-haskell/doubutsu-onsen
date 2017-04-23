{-# LANGUAGE DeriveGeneric #-}

module Data.DoubutsuOnsen.Doubutsu (
  Doubutsu (..), Coord (..), SlotStatus (..),
  ) where

import GHC.Generics (Generic)
import Data.Int (Int16)
-- import Data.Time (LocalTime)
import Data.Text (Text)


data Doubutsu a =
  Doubutsu
  { doubutsuId   :: Int16
  , doubutsuName :: Text
  --- , relasedAt    :: LocalTime
  , coordList    :: a
  } deriving (Eq, Show, Generic)

data Coord =
  Coord
  { relativeX :: Int16
  , relativeY :: Int16
  } deriving (Eq, Show, Generic)

data SlotStatus a =
  SlotStatus
  { locateX  :: Int16
  , locateY  :: Int16
  , doubutsu :: Doubutsu a
  } deriving (Eq, Show, Generic)
