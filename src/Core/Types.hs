module Core.Types where

import Control.Concurrent.STM
import Data.Time
import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map

data OrderType = 
    MarketBuy 
  | MarketSell 
  | LimitBuy 
  | LimitSell
  | StopBuy 
  | StopSell
  deriving (Show, Eq)

type Asset = String  

data Order = Order {
     orderId :: Int 
    , orderType :: OrderType
    , asset :: Asset
    , price :: Double
    , quantity :: Int 
    , timestamp :: UTCTime   
} deriving (Show, Eq)

type AssetOrders = Map.Map Asset (TVar [Order])
data OrderBook = OrderBook {
      bids :: AssetOrders
    , asks :: AssetOrders
    }