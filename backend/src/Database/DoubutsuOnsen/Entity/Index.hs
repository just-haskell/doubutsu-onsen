{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Index where

import GHC.Generics (Generic)
import Data.Int (Int16, Int32)
import Database.Relational.Query (Pi, (|$|), (|*|))
import Database.HDBC.Query.TH (makeRelationalRecord)

import Database.DoubutsuOnsen.Entity.Slot (Slot)
import qualified Database.DoubutsuOnsen.Entity.Slot as Slot
import Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu (SlotStatusDoubutsu)
import qualified Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu as StatusDoubutsu
import Database.DoubutsuOnsen.Entity.SlotStatusItem (SlotStatusItem)
import qualified Database.DoubutsuOnsen.Entity.SlotStatusItem as StatusItem


data SlotIndex =
  SlotIndex
  { slotOnsenId :: Int16
  , slotLocalSlotNumber :: Int16
  } deriving Generic

$(makeRelationalRecord ''SlotIndex)

data SlotStatusIndex =
  SlotStatusIndex
  { statusGameId :: Int32
  , statusOnsenId :: Int16
  , statusLocalSlotNumber :: Int16
  } deriving Generic

$(makeRelationalRecord ''SlotStatusIndex)

slot :: Pi Slot SlotIndex
slot = SlotIndex |$| Slot.onsenId' |*| Slot.localSlotNumber'

slotStatusDoubutsu :: Pi SlotStatusDoubutsu SlotStatusIndex
slotStatusDoubutsu =
  SlotStatusIndex
  |$| StatusDoubutsu.gameId'
  |*| StatusDoubutsu.onsenId'
  |*| StatusDoubutsu.localSlotNumber'

slotStatusItem :: Pi SlotStatusItem SlotStatusIndex
slotStatusItem =
  SlotStatusIndex
  |$| StatusItem.gameId'
  |*| StatusItem.onsenId'
  |*| StatusItem.localSlotNumber'

statusSlot :: Pi SlotStatusIndex SlotIndex
statusSlot = SlotIndex |$| statusOnsenId' |*| statusLocalSlotNumber'
