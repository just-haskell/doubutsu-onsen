{-# LANGUAGE DeriveGeneric #-}

module Data.DoubutsuOnsen.Item (
  Item (..), SlotStatus (..),
  ) where

import GHC.Generics (Generic)
import Data.Int (Int16)
import Data.Text (Text)


data Item =
  Item
  { itemId   :: Int16
  , itemName :: Text
  , rerity   :: Int16
  } deriving (Eq, Generic)

data SlotStatus =
  SlotStatus
  { locateX :: Int16
  , locateY :: Int16
  , item :: Item
  , growthLevel :: Int16
  } deriving (Eq, Generic)
