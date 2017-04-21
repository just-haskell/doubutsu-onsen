{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Onsen where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int64)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "onsen"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "onsen_seq"
  [''Generic])

instance SequenceDerivable OnsenSeq Int64 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable Onsen OnsenSeq Int64
