# Project Arbiter

CLI tool that will reach out to the Nomics API to grab all active crypto and then upload that to Algolia.

## Setup

1. Install Brittany for code formatting using `stack install brittany`

## Running

1. Compile with `make build` or `stack build`
2. Run with `make run` or `stack exec -- arbiter-exe` or pass flags like `stack exec -- arbiter-exe --verbose`

## Run tests

1. Run with `make test` or `stack test`
