module Database.DoubutsuOnsen.DataSource (
  connect,
  defineTable,
  ) where

import Language.Haskell.TH (Q, Dec, TypeQ, Name)
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Query.TH (defineTableFromDB)


dev :: Bool
dev = True

connect :: IO Connection
connect = connectPostgreSQL dsn
  where
    dsn
      | dev        =  "dbname=onsendev"
      | otherwise  =  "dbname=onsenprod"

connectCompileTime :: IO Connection
connectCompileTime = connectPostgreSQL "dbname=onsendev"

defineTable :: [(String, TypeQ)]
            -> String
            -> String
            -> [Name]
            -> Q [Dec]
defineTable tmap =
  defineTableFromDB
    connectCompileTime
    (driverPostgreSQL { typeMap = tmap })
