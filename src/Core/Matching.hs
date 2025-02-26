module Core.Matching where

import Core.Types
import Control.Concurrent.STM
import Data.List (sortBy)
import Data.Time.Clock
import qualified Data.Map as Map

-- Add MatchingAlgorithm type/ will be the interface to work with
data MatchingAlgorithm = 
    PriceTime              -- Regular price-time priority
  | Batch                 -- Batch matching
  deriving (Show, Eq)

data Trade = Trade {
    buyOrder :: Order,
    sellOrder :: Order,
    matchPrice :: Double,
    matchQuantity :: Int
} deriving (Show, Eq)

match :: MatchingAlgorithm -> Asset -> OrderBook -> UTCTime -> STM [Trade]
match PriceTime = matchPriceTime
match Batch = batchMatching

getBids :: OrderBook -> Asset -> TVar [Order]
getBids book asset = 
    case Map.lookup asset (bids book) of
        Just ordersVar -> ordersVar
        Nothing -> error "Asset not found in order book"

getAsks :: OrderBook -> Asset -> TVar [Order]
getAsks book asset =
    case Map.lookup asset (asks book) of
        Just ordersVar -> ordersVar 
        Nothing -> error "Asset not found in order book"

-- Takes time as parameter now
matchPriceTime :: Asset -> OrderBook -> UTCTime -> STM [Trade]
matchPriceTime asset book now = do
    bids <- readTVar (getBids book asset)
    asks <- readTVar (getAsks book asset)
    
    let sortedBids = sortOrders True bids
        sortedAsks = sortOrders False asks
    
    matchOrders now sortedBids sortedAsks

sortOrders :: Bool -> [Order] -> [Order]
sortOrders isBuy orders = 
    sortBy compareOrders orders
  where
    compareOrders o1 o2 = 
        case compare (price o1) (price o2) of
            EQ -> compare (timestamp o1) (timestamp o2)
            x -> if isBuy then invertCompare x else x

invertCompare :: Ordering -> Ordering
invertCompare LT = GT
invertCompare GT = LT
invertCompare EQ = EQ

-- Takes time as first parameter
matchOrders :: UTCTime -> [Order] -> [Order] -> STM [Trade]
matchOrders _ [] _ = return []
matchOrders _ _ [] = return []
matchOrders now (bid:restBids) (ask:restAsks) = 
    if canMatch bid ask 
        then do
            let (trade, remainingBid, remainingAsk) = createTrade bid ask now
            moreTrades <- case (remainingBid, remainingAsk) of
                (Just b, Just a) -> matchOrders now (b:restBids) (a:restAsks)
                (Just b, Nothing) -> matchOrders now (b:restBids) restAsks
                (Nothing, Just a) -> matchOrders now restBids (a:restAsks)
                (Nothing, Nothing) -> matchOrders now restBids restAsks
            return (trade : moreTrades)
        else return []

canMatch :: Order -> Order -> Bool
canMatch bid ask = case (orderType bid, orderType ask) of
    (MarketBuy, MarketSell) -> True
    (MarketBuy, LimitSell) -> True
    (LimitBuy, MarketSell) -> True
    (LimitBuy, LimitSell) -> price bid >= price ask
    (StopBuy, _) -> price bid >= price ask
    (_, StopSell) -> price bid >= price ask
    _ -> False

createTrade :: Order -> Order -> UTCTime -> (Trade, Maybe Order, Maybe Order)
createTrade bid ask now =
    let matchQty = min (quantity bid) (quantity ask)
        matchPrc = determinePrice bid ask
        
        remainingBid = if quantity bid > matchQty
            then Just bid{quantity = quantity bid - matchQty}
            else Nothing
            
        remainingAsk = if quantity ask > matchQty
            then Just ask{quantity = quantity ask - matchQty}
            else Nothing
            
        trade = Trade {
            buyOrder = bid,
            sellOrder = ask,
            matchPrice = matchPrc,
            matchQuantity = matchQty
        }
    in (trade, remainingBid, remainingAsk)

determinePrice :: Order -> Order -> Double
determinePrice bid ask = case (orderType bid, orderType ask) of
    (MarketBuy, MarketSell) -> (price bid + price ask) / 2
    (MarketBuy, _) -> price ask
    (_, MarketSell) -> price bid
    _ -> price ask


---- batch matching algo
batchMatching :: Asset -> OrderBook -> UTCTime -> STM [Trade]
batchMatching asset book now = do
    bids <- readTVar (getBids book asset)
    asks <- readTVar (getAsks book asset)
    
    let sortedBids = sortOrders True bids
        sortedAsks = sortOrders False asks
        clearingPrice = calculateClearingPrice sortedBids sortedAsks
    
    matchAtBatchPrice clearingPrice now sortedBids sortedAsks

calculateClearingPrice :: [Order] -> [Order] -> Double
calculateClearingPrice bids asks = 
    let -- Get all orders
        allOrders = bids ++ asks
        -- Calculate total volume (sum of quantities)
        totalVolume = sum (map quantity allOrders)
        -- Calculate weighted sum (price * quantity for each order)
        weightedSum = sum [price o * fromIntegral (quantity o) | o <- allOrders]
    in if totalVolume > 0 
        then weightedSum / fromIntegral totalVolume
        else 0.0

matchAtBatchPrice :: Double -> UTCTime -> [Order] -> [Order] -> STM [Trade]
matchAtBatchPrice price now bids asks = do
    let validBids = filter (canExecuteAtBatchPrice True price) bids
        validAsks = filter (canExecuteAtBatchPrice False price) asks
    createBatchTrades price now validBids validAsks

canExecuteAtBatchPrice :: Bool -> Double -> Order -> Bool
canExecuteAtBatchPrice isBuy clearingPrice order = case orderType order of
    MarketBuy -> isBuy
    MarketSell -> not isBuy
    LimitBuy -> isBuy && clearingPrice <= price order  
    LimitSell -> not isBuy && clearingPrice >= price order  
    _ -> False


createBatchTrades :: Double -> UTCTime -> [Order] -> [Order] -> STM [Trade]
createBatchTrades _ _ [] _ = return []
createBatchTrades _ _ _ [] = return []
createBatchTrades price now (bid:bids) (ask:asks) = do
    let matchQty = min (quantity bid) (quantity ask)
        trade = Trade {
            buyOrder = bid,
            sellOrder = ask,
            matchPrice = price,
            matchQuantity = matchQty
        }
        
        -- Handle remaining quantities
        remainingBid = if quantity bid > matchQty
            then [bid { quantity = quantity bid - matchQty }]
            else []
        remainingAsk = if quantity ask > matchQty
            then [ask { quantity = quantity ask - matchQty }]
            else []

    -- Recursively match remaining orders
    restTrades <- createBatchTrades price now 
                    (remainingBid ++ bids) 
                    (remainingAsk ++ asks)
    return (trade : restTrades)