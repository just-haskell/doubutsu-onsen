{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.SlotStatusDoubutsu where

import GHC.Generics (Generic)
import Prelude hiding (seq)

import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "slot_status_doubutsu"
  [''Eq, ''Show, ''Generic])
