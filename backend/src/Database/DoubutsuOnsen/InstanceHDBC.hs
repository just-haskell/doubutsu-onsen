{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.DoubutsuOnsen.InstanceHDBC () where

import Database.Record (FromSql)
import Database.HDBC (SqlValue)
import Database.HDBC.Record.Persistable ()

import Data.DoubutsuOnsen.Onsen (Onsen)
import qualified Data.DoubutsuOnsen.Onsen as Onsen
import Data.DoubutsuOnsen.Doubutsu (Doubutsu)
import qualified Data.DoubutsuOnsen.Doubutsu as Doubutsu
import Data.DoubutsuOnsen.Item (Item)
import qualified Data.DoubutsuOnsen.Item as Item

instance FromSql SqlValue Onsen
instance (FromSql SqlValue a, FromSql SqlValue b) => FromSql SqlValue (Onsen.Status a b)

instance FromSql SqlValue a => FromSql SqlValue (Doubutsu a)
instance FromSql SqlValue a => FromSql SqlValue (Doubutsu.SlotStatus a)
instance FromSql SqlValue Doubutsu.Coord

instance FromSql SqlValue Item
instance FromSql SqlValue Item.SlotStatus
