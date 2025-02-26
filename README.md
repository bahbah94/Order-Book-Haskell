# order-match
# Concurrent Order Matching Engine

A high-performance, concurrent order matching engine implemented in Haskell. This project demonstrates the use of Software Transactional Memory (STM) for thread-safe operations and multiple matching algorithms.

## Features

- Thread-safe order book management using STM
- Multiple matching algorithms:
  - Price-time priority matching
  - Batch matching with VWAP pricing
- Support for multiple assets
- Interactive command-line interface
- Comprehensive order types

## Order Types

The engine supports various order types:
- Market Buy/Sell
- Limit Buy/Sell
- Stop Buy/Sell

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- GHC (installed automatically by Stack)

### Building from Source

```bash
# Clone the repository
git clone https://github.com/bahbah94/Order-Book-Haskell.git
cd order-match

# Build the project
stack build

# Run the executable
stack exec order-match
```

## Usage

The engine provides an interactive command-line interface:

```
Order Matching Engine - Interactive Mode
> place --asset BTC --type limit-buy --price 50000 --quantity 2
Order placed successfully with ID: 1
> place --asset BTC --type limit-sell --price 50000 --quantity 1
Order placed successfully with ID: 2
Matches found:
Matched 1 units at price 50000.0
> show
=== Order Book ===

Asset: BTC
Bids:
Price		Quantity	Type
50000.0		1		LimitBuy

Asks:
Price		Quantity	Type

Asset: ETH
Bids:
Price		Quantity	Type
Asks:
Price		Quantity	Type
```

### Available Commands

- `place`: Place a new order with the following options:
  - `--asset`: Asset symbol (e.g., BTC, ETH)
  - `--type`: Order type (market-buy, market-sell, limit-buy, limit-sell, stop-buy, stop-sell)
  - `--price`: Order price
  - `--quantity`: Order quantity

- `show`: Display the current order book

- `exit`: Exit the program

## Project Structure

```
order-match/
├── src/
│   ├── Core/                   -- Core matching engine
│   │   ├── Types.hs           -- Basic data types
│   │   ├── Matching.hs        -- Matching algorithms
│   │   └── Engine.hs          -- Order processing
│   │
│   └── CLI/                   -- Command line interface
│       ├── Commands.hs        -- Command definitions
│       ├── Parser.hs          -- Command parsing
│       ├── State.hs           -- State management
│       └── Execute.hs         -- Command execution
│
├── app/                       -- Main application
│   └── Main.hs
│
└── test/                      -- Tests
```


## Future Development

- Simulation framework for load testing
- Performance metrics collection and visualization
- Persistence layer for order storage
- Network API for remote access
- Additional matching algorithms

