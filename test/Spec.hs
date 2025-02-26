module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Core.Types
import Core.Matching
import Core.Engine
import Control.Concurrent.STM
import Data.Time.Clock

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Order Matching Tests" 
    [ testPriceTimeMatching
    , testBatchMatching
    ]

-- Group price-time priority tests
testPriceTimeMatching :: TestTree
testPriceTimeMatching = testGroup "Price-Time Priority Tests"
    [ testOrderPlacement
    , testBasicOrdering
    , testMarketOrderMatching
    , testLimitOrderMatching
    ]

-- Group batch matching tests
testBatchMatching :: TestTree
testBatchMatching = testGroup "Batch Matching Tests"
    [ testBasicBatch
    , testBatchClearing
    , testBatchPartialFills
    ]

testOrderPlacement :: TestTree
testOrderPlacement = testCase "Basic order placement" $ do
    now <- getCurrentTime
    
    let order = Order {
        orderId = 1,
        orderType = MarketBuy,
        asset = "BTC",
        price = 100.0,
        quantity = 1,
        timestamp = now
    }
    
    result <- atomically $ do
        engine <- newEngine ["BTC"]
        placeOrder engine order

    result @?= 1

testBasicOrdering :: TestTree
testBasicOrdering = testCase "Order sorting" $ do
    now <- getCurrentTime
    
    let buy1 = Order 1 MarketBuy "BTC" 100.0 1 now
        buy2 = Order 2 LimitBuy "BTC" 98.0 1 now
        
        sortedOrders = sortOrders True [buy1, buy2]
    
    length sortedOrders @?= 2
    orderId (head sortedOrders) @?= 1

testMarketOrderMatching :: TestTree
testMarketOrderMatching = testCase "Market order matching" $ do
    now <- getCurrentTime
    
    let buyOrder = Order 1 MarketBuy "BTC" 100.0 1 now
        sellOrder = Order 2 MarketSell "BTC" 98.0 1 now
    
    trades <- atomically $ do
        engine <- newEngine ["BTC"]
        placeOrder engine buyOrder
        placeOrder engine sellOrder
        match PriceTime "BTC" (orderBook engine) now
    
    length trades @?= 1
    matchPrice (head trades) @?= 99.0  -- Mid price for market orders

testLimitOrderMatching :: TestTree
testLimitOrderMatching = testCase "Limit order matching" $ do
    now <- getCurrentTime
    
    let buyOrder = Order 1 LimitBuy "BTC" 100.0 1 now
        sellOrder = Order 2 LimitSell "BTC" 98.0 1 now
    
    trades <- atomically $ do
        engine <- newEngine ["BTC"]
        placeOrder engine buyOrder
        placeOrder engine sellOrder
        match PriceTime "BTC" (orderBook engine) now
    
    length trades @?= 1
    matchPrice (head trades) @?= 98.0  -- Should match at sell price
    matchQuantity (head trades) @?= 1

testBasicBatch :: TestTree
testBasicBatch = testCase "Basic batch matching" $ do
    now <- getCurrentTime
    
    let orders = [ Order 1 LimitBuy "BTC" 100.0 1 now
                , Order 2 LimitBuy "BTC" 99.0 1 now
                , Order 3 LimitSell "BTC" 98.0 1 now
                , Order 4 LimitSell "BTC" 97.0 1 now
                ]
    
    trades <- atomically $ do
        engine <- newEngine ["BTC"]
        mapM_ (placeOrder engine) orders
        match Batch "BTC" (orderBook engine) now
    
    length trades @?= 2  -- Should match all orders
    let avgPrice = sum (map matchPrice trades) / fromIntegral (length trades)
    avgPrice @?= 98.5  -- Should be average of all prices

testBatchClearing :: TestTree
testBatchClearing = testCase "Batch clearing price calculation" $ do
    now <- getCurrentTime
    
    let buyOrders = [ Order 1 LimitBuy "BTC" 100.0 2 now  -- Higher quantity
                    , Order 2 LimitBuy "BTC" 99.0 1 now
                    ]
        sellOrders = [ Order 3 LimitSell "BTC" 98.0 1 now
                    , Order 4 LimitSell "BTC" 97.0 1 now
                    ]
    
    trades <- atomically $ do
        engine <- newEngine ["BTC"]
        mapM_ (placeOrder engine) (buyOrders ++ sellOrders)
        match Batch "BTC" (orderBook engine) now
    
    -- Clearing price should be weighted by quantity
    matchPrice (head trades) @?= 98.8  -- Fixed @?~ to @?=

testBatchPartialFills :: TestTree
testBatchPartialFills = testCase "Batch partial fills" $ do
    now <- getCurrentTime
    
    let buyOrder = Order 1 LimitBuy "BTC" 100.0 3 now    -- Wants to buy 3
        sellOrders = [ Order 2 LimitSell "BTC" 98.0 1 now
                    , Order 3 LimitSell "BTC" 98.0 1 now
                    ]
    
    trades <- atomically $ do
        engine <- newEngine ["BTC"]
        placeOrder engine buyOrder
        mapM_ (placeOrder engine) sellOrders
        match Batch "BTC" (orderBook engine) now
    
    length trades @?= 2  -- Should create two trades
    sum (map matchQuantity trades) @?= 2  -- Total quantity matched should be 2