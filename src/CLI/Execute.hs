module CLI.Execute where

import qualified Core.Types as T
import Core.Matching
import qualified CLI.Commands as C
import qualified CLI.State as S
import Core.Engine
import Control.Concurrent.STM
import Data.Time.Clock
import Control.Monad (forM_)

-- Single display function for order book
displayOrderBook :: OrderEngine -> IO ()
displayOrderBook engine = do
    putStrLn "\n=== Order Book ===\n"
    let book = orderBook engine
    
    -- Get all data in ONE atomic transaction
    orderData <- atomically $ do
        btcBids <- readTVar $ getBids book "BTC"
        btcAsks <- readTVar $ getAsks book "BTC"
        ethBids <- readTVar $ getBids book "ETH"
        ethAsks <- readTVar $ getAsks book "ETH"
        return [ ("BTC", reverse $ sortOrders True btcBids, sortOrders False btcAsks)
              , ("ETH", reverse $ sortOrders True ethBids, sortOrders False ethAsks)
              ]

    -- Display the data
    forM_ orderData $ \(asset, bids, asks) -> do
        putStrLn $ "\nAsset: " ++ asset
        putStrLn "Bids:"
        putStrLn "Price\t\tQuantity\tType"
        mapM_ displayOrder bids
        putStrLn "\nAsks:"
        putStrLn "Price\t\tQuantity\tType"
        mapM_ displayOrder asks

displayOrder :: T.Order -> IO ()
displayOrder order = putStrLn $ 
    show (T.price order) ++ "\t\t" ++ 
    show (T.quantity order) ++ "\t\t" ++ 
    show (T.orderType order)

-- Main command execution
executeCommand :: S.CliState -> C.Command -> IO S.CliState
executeCommand state cmd = case cmd of
    C.Place opts -> do
        now <- getCurrentTime
        let order = T.Order {
            T.orderId = 1,
            T.orderType = C.orderType opts,
            T.asset = C.asset opts,
            T.price = C.price opts,
            T.quantity = C.quantity opts,
            T.timestamp = now
        }
        
        -- Place order and handle matches in one atomic transaction
        (orderId, matches) <- atomically $ do
            id <- placeOrder (S.engineState state) order
            -- Try to match the order
            trades <- match PriceTime (C.asset opts) (orderBook $ S.engineState state) now
            return (id, trades)
            
        -- Display results
        putStrLn $ "Order placed successfully with ID: " ++ show orderId
        if not (null matches)
            then do
                putStrLn "Matches found:"
                forM_ matches $ \trade -> 
                    putStrLn $ "Matched " ++ show (matchQuantity trade) ++ 
                              " units at price " ++ show (matchPrice trade)
            else putStrLn "No immediate matches found"
        
        -- Show updated order book
        displayOrderBook (S.engineState state)
        return state

    C.Show -> do
        displayOrderBook (S.engineState state)
        return state