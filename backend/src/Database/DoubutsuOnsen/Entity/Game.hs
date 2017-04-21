{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Game where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int64)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "game"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "game_seq"
  [''Generic])

instance SequenceDerivable GameSeq Int64 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable Game GameSeq Int64
