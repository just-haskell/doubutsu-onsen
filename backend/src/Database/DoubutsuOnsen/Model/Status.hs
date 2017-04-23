{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.DoubutsuOnsen.Model.Status (
  relSlotDoubutsu,
  relSlotItem,
  relOnsenStatus,
  ) where

import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Relational.Query

import Data.DoubutsuOnsen.Onsen (Onsen (Onsen))
import qualified Data.DoubutsuOnsen.Onsen as Onsen
import Data.DoubutsuOnsen.Doubutsu (Doubutsu (Doubutsu))
import qualified Data.DoubutsuOnsen.Doubutsu as Doubutsu
import Data.DoubutsuOnsen.Item (Item (Item))
import qualified Data.DoubutsuOnsen.Item as Item

import Database.DoubutsuOnsen.Entity.Onsen (onsen)
import qualified Database.DoubutsuOnsen.Entity.Onsen as OnsenE
import Database.DoubutsuOnsen.Entity.OnsenStatus (onsenStatus)
import qualified Database.DoubutsuOnsen.Entity.OnsenStatus as OnsenStatusE
import Database.DoubutsuOnsen.Entity.Slot (slot)
import qualified Database.DoubutsuOnsen.Entity.Slot as SlotE
import Database.DoubutsuOnsen.Entity.Doubutsu (doubutsu)
import qualified Database.DoubutsuOnsen.Entity.Doubutsu as DoubutsuE
import Database.DoubutsuOnsen.Entity.DoubutsuCoord (doubutsuCoord)
import qualified Database.DoubutsuOnsen.Entity.DoubutsuCoord as DoubutsuCoordE
import Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu (slotStatusDoubutsu)
import qualified Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu as SlotStatusDoubutsuE
import Database.DoubutsuOnsen.Entity.Item (item)
import qualified Database.DoubutsuOnsen.Entity.Item as ItemE
import Database.DoubutsuOnsen.Entity.SlotStatusItem (slotStatusItem)
import qualified Database.DoubutsuOnsen.Entity.SlotStatusItem as SlotStatusItemE
import qualified Database.DoubutsuOnsen.Entity.Index as Index


instance ShowConstantTermsSQL ()

instance ProductConstructor (Int16 -> Text -> LocalTime -> a -> Doubutsu a) where
  productConstructor = Doubutsu

instance ProductConstructor (Int16 -> Int16 -> Doubutsu a -> Doubutsu.SlotStatus a) where
  productConstructor = Doubutsu.SlotStatus

instance ProductConstructor (Int16 -> Int16 -> Doubutsu.Coord) where
  productConstructor = Doubutsu.Coord

instance ProductConstructor (Int16 -> Text -> Int16 -> Item) where
  productConstructor = Item

instance ProductConstructor (Int16 -> Int16 -> Item -> Int16 -> Item.SlotStatus) where
  productConstructor = Item.SlotStatus

instance ProductConstructor (Int16 -> Text -> LocalTime -> Onsen) where
  productConstructor = Onsen

instance ProductConstructor
         (Onsen
          -> Int16
          -> Int16
          -> Int32
          -> LocalTime
          -> LocalTime
          -> a
          -> b
          -> Onsen.Status a b) where
  productConstructor = Onsen.Status


relSlotDoubutsu :: Relation (Int32, Int16) (Doubutsu.SlotStatus Doubutsu.Coord)
relSlotDoubutsu = relation' $ do
  s  <- query slot
  ss <- query slotStatusDoubutsu
  on $ s ! Index.slot .=. ss ! Index.slotStatusDoubutsu ! Index.statusSlot
  on $ s ! SlotE.slotType' .=. value 0
  d  <- query doubutsu
  on $ ss ! SlotStatusDoubutsuE.doubutsuId' .=. just (d ! DoubutsuE.id')
  dc <- query doubutsuCoord
  on $ d ! DoubutsuE.id' .=. dc ! DoubutsuCoordE.doubutsuId'

  (gph, ()) <- placeholder $ \ph' -> wheres $ ss ! SlotStatusDoubutsuE.gameId' .=. ph'
  (oph, ()) <- placeholder $ \ph' -> wheres $ ss ! SlotStatusDoubutsuE.onsenId' .=. ph'

  orderBy (s ! SlotE.localSlotNumber') Asc
  orderBy (dc ! DoubutsuCoordE.localCoordNumber') Asc

  let coordM = Doubutsu.Coord |$| dc ! DoubutsuCoordE.relativeX' |*| dc ! DoubutsuCoordE.relativeY'
      doubutsuM = Doubutsu |$| d ! DoubutsuE.id' |*| d ! DoubutsuE.doubutsuName' |*| d ! DoubutsuE.releasedAt' |*| coordM
      st = Doubutsu.SlotStatus |$| s ! SlotE.locateX' |*| s ! SlotE.locateY' |*| doubutsuM

  return (gph >< oph, st)

relSlotItem :: Relation (Int32, Int16) Item.SlotStatus
relSlotItem = relation' $ do
  s  <- query slot
  ss <- query slotStatusItem
  on $ s ! Index.slot .=. ss ! Index.slotStatusItem ! Index.statusSlot
  on $ s ! SlotE.slotType' .=. value 1
  i  <- query item
  on $ ss ! SlotStatusItemE.itemId' .=. just (i ! ItemE.id')

  (gph, ()) <- placeholder $ \ph' -> wheres $ ss ! SlotStatusItemE.gameId' .=. ph'
  (oph, ()) <- placeholder $ \ph' -> wheres $ ss ! SlotStatusItemE.onsenId' .=. ph'

  orderBy (s ! SlotE.localSlotNumber') Asc

  let itemM = Item |$| i ! ItemE.id' |*| i ! ItemE.itemName' |*| i ! ItemE.rarity'
      st = Item.SlotStatus |$| s ! SlotE.locateX' |*| s ! SlotE.locateY' |*| itemM |*| ss ! SlotStatusItemE.growthLevel'

  return (gph >< oph, st)

relOnsenStatus :: Relation (Int32, Int16) (Onsen.Status () ())
relOnsenStatus = relation' $ do
  o  <- query onsen
  os <- query onsenStatus
  on $ o ! OnsenE.id' .=. os ! OnsenStatusE.onsenId'

  (gph, ()) <- placeholder $ \ph -> wheres $ os ! OnsenStatusE.gameId' .=. ph
  (oph, ()) <- placeholder $ \ph -> wheres $ os ! OnsenStatusE.onsenId' .=. ph

  let onsenM = Onsen |$| o ! OnsenE.id' |*| o ! OnsenE.onsenName' |*| o ! OnsenE.releasedAt'
      st = Onsen.Status
           |$| onsenM
           |*| os ! OnsenStatusE.onsenLevel'
           |*| os ! OnsenStatusE.missionStatus'
           |*| os ! OnsenStatusE.seed'
           |*| os ! OnsenStatusE.updatedAt'
           |*| os ! OnsenStatusE.startedAt'
           |*| value ()
           |*| value ()

  return (gph >< oph, st)
