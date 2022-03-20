### [1.0.0] - UNRELEASED

#### Added

- [📌 #1](https://github.com/CardanoSolutions/kupo/issues/1) - New API endpoint to get application's health, 
  - `GET v1/health` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getHealth)

- [📌 #1](https://github.com/CardanoSolutions/kupo/issues/1) - New command `healthcheck` to perform a health check against a running server. Handy when combined with Docker's HEALTHCHECK feature.
- [📌 #9](https://github.com/CardanoSolutions/kupo/issues/9) - Add `header_hash` to each match, so that data can easily be queried from [Ogmios](https://github.com/CardanoSolutions/ogmios) using the chain-sync protocol (which requires _points_ defined as both slot number and header hashes!).

- [📌 #11](https://github.com/CardanoSolutions/kupo/issues/11) - Support for [Ogmios](https://github.com/CardanoSolutions/ogmios) as an alternative chain-sync provider. This is particularly interesting when used with remote instances. This is activated by providing the relevant command-line options instead of `--node-socket` and `--node-config`:
  - `--ogmios-host`
  - `--ogmios-port`

#### Changed

- [📌 #2](https://github.com/CardanoSolutions/kupo/issues/2) - The command-line is now more idempotent, restarting the server with the same options will no longer fail and simply resume syncing.

- [📌 #13](https://github.com/CardanoSolutions/kupo/issues/13) - Kupo no longer ignores Byron blocks internally and will now also synchronizes data from them. 

### [1.0.0-beta] - 2022-02-27

#### Added

- Initial (beta) release:
  - Synchronization from chosen point (`--since POINT`)

  - Storage in-memory or on-disk (`--workdir DIR | --in-memory`)

  - Multiple patterns via the command-line (`--match`)

  - Basic API access:
    - `GET v1/matches` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches)
    - `GET v1/matches/{pattern-fragment}` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatchesUnary)
    - `GET v1/matches/{pattern-fragment}/{pattern-fragment}` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches2Ary)
    - `GET v1/checkoints` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getCheckpoints)

  - Multi-level (basic) structured component logging (`--log-level`, `log-level-{component}`)

- Current limitations:
  
  - Byron blocks are ignored;
  - The HTTP server does not support `OPTION` and `HEAD` requests;
  - Lack of proper integration testing;

#### Changed

N/A

#### Removed

N/A
