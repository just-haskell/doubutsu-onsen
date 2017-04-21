{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Gameuser where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int64)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "gameuser"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "gameuser_seq"
  [''Generic])

instance SequenceDerivable GameuserSeq Int64 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable Gameuser GameuserSeq Int64
