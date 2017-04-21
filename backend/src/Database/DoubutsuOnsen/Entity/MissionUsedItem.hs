{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.MissionUsedItem where

import GHC.Generics (Generic)
import Prelude hiding (seq)

import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "mission_used_item"
  [''Eq, ''Show, ''Generic])
