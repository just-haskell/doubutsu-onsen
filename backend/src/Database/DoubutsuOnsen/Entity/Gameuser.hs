{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Gameuser where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int32)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, SequenceFromTable (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "gameuser"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "gameuser_seq"
  [''Generic])

instance SequenceDerivable GameuserSeq Int32 where
  deriveSequence = unsafeSpecifySequence seq seq'

instance SequenceFromTable Gameuser GameuserSeq Int32
