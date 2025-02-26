module Core.Engine where

import Core.Types
import Core.Matching
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Time.Clock

data OrderEngine = OrderEngine { 
    orderBook :: OrderBook
}

-- Now takes list of assets
newEngine :: [Asset] -> STM OrderEngine
newEngine assets = do
    emptyBook <- newOrderBook assets  -- Pass assets to newOrderBook
    return $ OrderEngine emptyBook

newOrderBook :: [Asset] -> STM OrderBook
newOrderBook assets = do
    let makeEmptyTVar = newTVar []
    bidTVars <- mapM (\_ -> makeEmptyTVar) assets
    askTVars <- mapM (\_ -> makeEmptyTVar) assets
    
    let bidsMap = Map.fromList (zip assets bidTVars)
        asksMap = Map.fromList (zip assets askTVars)  -- Fixed: was using bidTVars
    
    return $ OrderBook bidsMap asksMap

-- Rest remains similar but needs to verify asset exists
placeOrder :: OrderEngine -> Order -> STM Int
placeOrder engine order = do
    -- First verify asset is supported
    case (Map.lookup (asset order) (bids book), 
          Map.lookup (asset order) (asks book)) of
        (Just _, Just _) -> do  -- Asset exists in both maps
            case orderType order of
                MarketBuy -> addToBook (bids book) order
                MarketSell -> addToBook (asks book) order
                LimitBuy -> addToBook (bids book) order
                LimitSell -> addToBook (asks book) order
                StopBuy -> addToBook (bids book) order
                StopSell -> addToBook (asks book) order
            
            trades <- matchPriceTime (asset order) book (timestamp order)
            processMatches trades
            return (orderId order)
        _ -> error $ "Asset " ++ asset order ++ " not supported"
  where
    book = orderBook engine

-- Rest remains the same
addToBook :: AssetOrders -> Order -> STM ()
addToBook bookSide order = do
    case Map.lookup (asset order) bookSide of
        Just ordersVar -> do
            orders <- readTVar ordersVar
            writeTVar ordersVar (order:orders)
        Nothing -> error "Asset not supported"

processMatches :: [Trade] -> STM ()
processMatches trades = do
    return ()