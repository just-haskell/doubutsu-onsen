module System.DoubutsuOnsen.Query (
  queryOnsenStatus,
  ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (groupBy)
import Data.Int (Int16, Int32)
import Database.HDBC.Session (withConnectionIO')
import Database.HDBC.Record (runQuery')
import Database.Relational.Query (relationalQuery)

import Data.DoubutsuOnsen.Onsen (Status (..))
import Data.DoubutsuOnsen.Doubutsu (Doubutsu (..), SlotStatus (..))
import qualified Data.DoubutsuOnsen.Doubutsu as Doubutsu
import qualified Data.DoubutsuOnsen.Item as Item
import Database.DoubutsuOnsen.DataSource (connect)
import Database.DoubutsuOnsen.InstanceHDBC ()
import Database.DoubutsuOnsen.Model.Status
  (relSlotDoubutsu, relSlotItem, relOnsenStatus, )


queryOnsenStatus :: Int32
                 -> Int16
                 -> IO (Status [Doubutsu.SlotStatus [Doubutsu.Coord]] [Item.SlotStatus])
queryOnsenStatus gameId onsenId = withConnectionIO' connect $ \conn -> do
  let ix = (gameId, onsenId)
  ss0  <- runQuery' conn (relationalQuery relOnsenStatus) ix
  ds0  <- runQuery' conn (relationalQuery relSlotDoubutsu) ix
  is0  <- runQuery' conn (relationalQuery relSlotItem) ix

  let integrateD g = const (map (coordList . doubutsu) g) <$> head g
      ds = map integrateD $ groupBy ((==) `on` doubutsuId . doubutsu) ds0

  case ss0 of
    []    ->  fail "onsen status not found!"
    [ss1] ->  return $ ss1 { doubutsuSlots = ds, itemSlots = is0 }
    _:_   ->  fail "more than one onsen status exist!"
