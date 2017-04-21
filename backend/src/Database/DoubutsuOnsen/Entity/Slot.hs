{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Slot where

import GHC.Generics (Generic)
import Prelude hiding (seq)

import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "slot"
  [''Eq, ''Show, ''Generic])
