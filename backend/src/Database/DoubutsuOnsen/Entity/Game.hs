{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Game where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int32)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "game"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "game_seq"
  [''Generic])

instance SequenceDerivable GameSeq Int32 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable Game GameSeq Int32
