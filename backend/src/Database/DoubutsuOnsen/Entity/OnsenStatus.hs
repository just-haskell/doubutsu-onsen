{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.OnsenStatus where

import GHC.Generics (Generic)
import Prelude hiding (seq)

import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "onsen_status"
  [''Eq, ''Show, ''Generic])
