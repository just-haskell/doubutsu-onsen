{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.DoubutsuOnsen.Model.Status (
  relSlotStatus
  ) where

import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Relational.Query

-- import Data.DoubutsuOnsen.Onsen
import Data.DoubutsuOnsen.Doubutsu (Doubutsu (Doubutsu))
import qualified Data.DoubutsuOnsen.Doubutsu as Doubutsu
import Database.DoubutsuOnsen.Entity.Slot (slot)

import qualified Database.DoubutsuOnsen.Entity.Slot as SlotE
import Database.DoubutsuOnsen.Entity.Doubutsu (doubutsu)
import qualified Database.DoubutsuOnsen.Entity.Doubutsu as DoubutsuE
import Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu
  (slotStatusDoubutsu)
import qualified Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu as SlotStatusDoubutsuE
import Database.DoubutsuOnsen.Entity.SlotStatusItem
  (slotStatusItem)
import qualified Database.DoubutsuOnsen.Entity.SlotStatusItem as SlotStatusItemE
import qualified Database.DoubutsuOnsen.Entity.Index as Index


instance ShowConstantTermsSQL ()

instance ProductConstructor (Int16 -> Text -> LocalTime -> a -> Doubutsu a) where
  productConstructor = Doubutsu

instance ProductConstructor (Int16 -> Int16 -> Doubutsu a -> Doubutsu.SlotStatus a) where
  productConstructor = Doubutsu.SlotStatus

relSlotStatus :: Relation (Int32, Int16) (Doubutsu.SlotStatus ())
relSlotStatus = relation' $ do
  s  <- query slot
  ss <- query slotStatusDoubutsu
  on $ s ! Index.slot .=. ss ! Index.slotStatusDoubutsu ! Index.statusSlot
  d  <- query doubutsu
  on $ ss ! SlotStatusDoubutsuE.doubutsuId' .=. just (d ! DoubutsuE.id')

  (gph, ()) <- placeholder $ \ph' -> wheres $ ss ! SlotStatusDoubutsuE.gameId' .=. ph'
  (oph, ()) <- placeholder $ \ph' -> wheres $ ss ! SlotStatusDoubutsuE.onsenId' .=. ph'

  orderBy (s ! SlotE.localSlotNumber') Asc

  let doubutsuM = Doubutsu |$| d ! DoubutsuE.id' |*| d ! DoubutsuE.doubutsuName' |*| d ! DoubutsuE.releasedAt' |*| value ()
      st = Doubutsu.SlotStatus |$| s ! SlotE.locateX' |*| s ! SlotE.locateY' |*| doubutsuM

  return (gph >< oph, st)
