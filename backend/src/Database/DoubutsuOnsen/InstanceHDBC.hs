{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.DoubutsuOnsen.InstanceHDBC () where

import Database.Record (FromSql)
import Database.HDBC (SqlValue)
import Database.HDBC.Record.Persistable ()

import Data.DoubutsuOnsen.Doubutsu (Doubutsu)


instance FromSql SqlValue a => FromSql SqlValue (Doubutsu a)
