-- CLI/Parser.hs
module CLI.Parser where

import Options.Applicative
import CLI.Commands
import qualified Core.Types as T

commandParser :: Parser Command
commandParser = subparser
    ( command "place" (info (Place <$> placeOrderParser) (progDesc "Place a new order"))
    <> command "show"  (info (pure Show) (progDesc "Show order book"))
    )

placeOrderParser :: Parser PlaceOrderOpts
placeOrderParser = PlaceOrderOpts
    <$> strOption 
        ( long "asset"
        <> help "Asset to trade (e.g., BTC)" )
    <*> orderTypeParser
    <*> option auto
        ( long "price"
        <> help "Order price" )
    <*> option auto
        ( long "quantity"
        <> help "Order quantity" )

orderTypeParser :: Parser T.OrderType
orderTypeParser = option readOrderType
    ( long "type"
    <> help "Order type (market-buy, market-sell, limit-buy, limit-sell, stop-buy, stop-sell)" )
  where
    readOrderType = eitherReader $ \s -> case s of
        "market-buy"  -> Right T.MarketBuy
        "market-sell" -> Right T.MarketSell
        "limit-buy"   -> Right T.LimitBuy
        "limit-sell"  -> Right T.LimitSell
        "stop-buy"    -> Right T.StopBuy
        "stop-sell"   -> Right T.StopSell
        _ -> Left "Invalid order type"