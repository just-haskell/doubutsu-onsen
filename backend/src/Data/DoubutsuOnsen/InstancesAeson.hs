{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.DoubutsuOnsen.InstancesAeson () where

import Data.Aeson (ToJSON, FromJSON)

import Data.DoubutsuOnsen.Onsen (Onsen)
import qualified Data.DoubutsuOnsen.Onsen as Onsen
import Data.DoubutsuOnsen.Doubutsu (Doubutsu)
import qualified Data.DoubutsuOnsen.Doubutsu as Doubutsu
import Data.DoubutsuOnsen.Item (Item)
import qualified Data.DoubutsuOnsen.Item as Item


instance ToJSON Onsen
instance FromJSON Onsen
instance (ToJSON a, ToJSON b) => ToJSON (Onsen.Status a b)
instance (FromJSON a, FromJSON b) => FromJSON (Onsen.Status a b)
instance ToJSON a => ToJSON (Doubutsu a)
instance FromJSON a => FromJSON (Doubutsu a)
instance ToJSON a => ToJSON (Doubutsu.SlotStatus a)
instance FromJSON a => FromJSON (Doubutsu.SlotStatus a)
instance ToJSON Doubutsu.Coord
instance FromJSON Doubutsu.Coord
instance ToJSON Item
instance FromJSON Item
instance ToJSON Item.SlotStatus
instance FromJSON Item.SlotStatus
