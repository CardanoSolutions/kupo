## Current Version

**4**

## Overview

<p align="center"><img src="schema.png" alt="database schema" /></p>

## Changelog

### Migration to `version=4`

- Removes all matches of mainnet & testnet transactions that failed phase-2 validations and consumed collaterals. 

### Migration to `version=3`

- Add a new `patterns` table to support (a) persisting patterns configuration and (b) managing patterns dynamically via HTTP.

### Migration to `version=2`

- Add a new `header_hash` column to the inputs table (to associate matches with fully qualified points made of slot and block header hash). 
