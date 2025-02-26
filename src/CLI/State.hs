-- CLI/State.hs
module CLI.State where

import qualified Core.Types as T
import Core.Engine
import Control.Concurrent.STM

data CliState = CliState {
    engineState :: OrderEngine,
    supportedAssets :: [String]
}

initState :: IO CliState
initState = atomically $ do
    eng <- newEngine ["BTC", "ETH"]  -- Use newEngine instead of newOrderBook
    return $ CliState eng ["BTC", "ETH"]