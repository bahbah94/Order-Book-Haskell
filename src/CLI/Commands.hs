module CLI.Commands where
import qualified Core.Types as T

data Command = 
    Place PlaceOrderOpts 
  | Show
  deriving (Show)

data PlaceOrderOpts = PlaceOrderOpts {
    asset    :: String,
    orderType:: T.OrderType,
    price    :: Double,
    quantity :: Int 
} deriving (Show)