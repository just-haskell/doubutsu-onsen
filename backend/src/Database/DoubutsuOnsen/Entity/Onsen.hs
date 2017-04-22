{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Onsen where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int32)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, primaryBindTriple, BindTableToSequence (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "onsen"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "onsen_seq"
  [''Generic])

instance SequenceDerivable OnsenSeq Int32 where
  derivedSequence = unsafeSpecifySequence seq seq'

instance BindTableToSequence Onsen OnsenSeq Int32 where
  bindTriple = primaryBindTriple
