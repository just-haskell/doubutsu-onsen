{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.EventLog where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int64)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "event_log"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "event_log_seq"
  [''Generic])

instance SequenceDerivable EventLogSeq Int64 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable EventLog EventLogSeq Int64
