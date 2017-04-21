{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Doubutsu where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int32)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "doubutsu"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "doubutsu_seq"
  [''Generic])

instance SequenceDerivable DoubutsuSeq Int32 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable Doubutsu DoubutsuSeq Int32
