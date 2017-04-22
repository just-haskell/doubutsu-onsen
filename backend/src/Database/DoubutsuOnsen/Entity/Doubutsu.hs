{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Doubutsu where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int16)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, primaryBindTriple, BindTableToSequence (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "doubutsu"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "doubutsu_seq"
  [''Generic])

instance SequenceDerivable DoubutsuSeq Int16 where
  derivedSequence = unsafeSpecifySequence seq seq'

instance BindTableToSequence Doubutsu DoubutsuSeq Int16 where
  bindTriple = primaryBindTriple
