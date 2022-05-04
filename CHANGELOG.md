### [1.0.1] - 2022-05-04

#### Added

- N/A

#### Changed

- [📌 #17](https://github.com/CardanoSolutions/kupo/discussions/17) - The internal reconnection logic and chain provider error handling has been reworked to be more resilient. In particular, before this patch, Kupo would re-synchronize the index from the provided configuration point in case of a connection lost and recovered with the chain provider. Now, it restarts where it was
before the connection was lost. Also, for Ogmios, few exceptions weren't properly caught and would simply cause the server to crash when loosing connection. 

- Fixed a log message informing about ongoing migration, communicating a wrong target version being migrated to (despite doing migration correctly).

#### Removed

- The server will no longer match on failed Alonzo transactions. That is, the index is only constructed out of successful and valid transactions. On Mainnet, this only concerns one transaction, but this could lead to quite surprising behavior should an application be watching the chain via Kupo as an index. Consequently, `1.0.1` includes [an automatic database migration](https://github.com/CardanoSolutions/kupo/blob/3d1ed9be402a7fc2129035d5f98fbb9b9a060c93/db/004.sql) which will prune any concerned output reference from the database for both _mainnet_ and _testnet_.

### [1.0.0] - 2022-03-27

#### Added

- [📌 #1](https://github.com/CardanoSolutions/kupo/issues/1) - New API endpoint to get application's health, 
  - `GET v1/health` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getHealth)

- [📌 #1](https://github.com/CardanoSolutions/kupo/issues/1) - New command `healthcheck` to perform a health check against a running server. Handy when combined with Docker's HEALTHCHECK feature.
- [📌 #9](https://github.com/CardanoSolutions/kupo/issues/9) - Add `header_hash` to each match, so that data can easily be queried from [Ogmios](https://github.com/CardanoSolutions/ogmios) using the chain-sync protocol (which requires _points_ defined as both slot number and header hashes!).

- [📌 #11](https://github.com/CardanoSolutions/kupo/issues/11) - Support for [Ogmios](https://github.com/CardanoSolutions/ogmios) as an alternative chain-sync provider. This is particularly interesting when used with remote instances. This is activated by providing the relevant command-line options instead of `--node-socket` and `--node-config`:
  - `--ogmios-host`
  - `--ogmios-port`

- [📌 #3](https://github.com/CardanoSolutions/kupo/issues/3) - Support for dynamic management (via HTTP API) of patterns. It is not possible to add, remove and list existing patterns via the HTTP API:
    - `GET v1/patterns` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getPatterns)
    - `DELETE v1/patterns/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deletePattern1Ary)
    - `DELETE v1/patterns/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deletePattern1Ary)
    - `PUT v1/patterns/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPattern1Ary)
    - `PUT v1/patterns/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPattern1Ary)

- [📌 #10](https://github.com/CardanoSolutions/kupo/issues/10) - Allow purging old matches (via HTTP API). This is however only allowed if the provided pattern is strictly non overlapping with an existing (active) pattern.
    - `DELETE v1/matches/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deleteMatch1Ary)
    - `DELETE v1/matches/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deleteMatch1Ary)

#### Changed

- [📌 #2](https://github.com/CardanoSolutions/kupo/issues/2) - The command-line is now more idempotent, restarting the server with the same options will no longer fail and simply resume syncing.

- [📌 #13](https://github.com/CardanoSolutions/kupo/issues/13) - Kupo no longer ignores Byron blocks internally and will now also synchronizes data from them. 

- [📌 #8](https://github.com/CardanoSolutions/kupo/issues/8) - Protects against restarting with different, non-compatible patterns. In case one restart the server with different patterns that those previously provided, it'll abort and crash. Note however that it's possible to restart the server with an empty patterns set, in which case it uses the previously known ones.

### [1.0.0-beta] - 2022-02-27

#### Added

- Initial (beta) release:
  - Synchronization from chosen point (`--since POINT`)

  - Storage in-memory or on-disk (`--workdir DIR | --in-memory`)

  - Multiple patterns via the command-line (`--match`)

  - Basic API access:
    - `GET v1/matches` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches)
    - `GET v1/matches/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches1Ary)
    - `GET v1/matches/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches2Ary)
    - `GET v1/checkoints` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getCheckpoints)

  - Multi-level (basic) structured component logging (`--log-level`, `log-level-{component}`)

- Current limitations:
  
  - Byron blocks are ignored;
  - The HTTP server does not support `OPTION` and `HEAD` requests;
  - Lack of proper integration testing;

#### Changed

N/A

#### Removed

N/A
