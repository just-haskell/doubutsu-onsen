{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module Database.DoubutsuOnsen.Entity.Item where

import GHC.Generics (Generic)
import Prelude hiding (seq)
import Data.Int (Int16)

import Database.Relational.Extra.Sequence
  (SequenceDerivable (..), unsafeSpecifySequence, primaryBindTriple, BindTableToSequence (..))
import Database.DoubutsuOnsen.DataSource (defineTable)

$(defineTable [] "DOUBUTSU" "item"
  [''Eq, ''Show, ''Generic])

$(defineTable [] "DOUBUTSU" "item_seq"
  [''Generic])

instance SequenceDerivable ItemSeq Int16 where
  derivedSequence = unsafeSpecifySequence seq seq'

instance BindTableToSequence Item ItemSeq Int16 where
  bindTriple = primaryBindTriple
