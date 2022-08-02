### [2.0.0] - UNRELEASED

> ⚠️  Breaking-Changes ⚠️ 
>
> This release contains important changes in the database structure and is
> therefore not compatible with previous releases. A full re-synchronization of
> the index will be needed. 
>
> Note also that any intermediate work from `master` isn't guaranteed to be 
> compatible with `v2.0.0`. Should you have been using intermediate edge versions, 
> you will need, in all likelihood, to drop and reconstruct the database as well.

#### Added

- [📌 #28](https://github.com/CardanoSolutions/kupo/issues/28) - Support for synchronization through the Babbage's era, including capturing inline-datums & reference scripts. 

- [📌 #17](https://github.com/CardanoSolutions/kupo/issues/20) - New command-line flag: `--prune-utxo`. When set, inputs that are spent on-chain will be removed from the index. Once-synced, 
  the index therefore only contain the current ledger UTxO set. When not set, spent inputs are kept in the index but are now marked accordingly to record if and when they've spent.

  HTTP endpoints for `/matches` (& the like) can also now accept either optional query-flag `?spent` or `?unspent` to filter matches depending on whether they've been spent. 

  Consequently, there's also a new (possibly `null`) field `spent_at` returned for each match result. When set, it indicates the slot in which the input was found being spent. 

  - `GET v1/matches[?(spent|unspent)]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches)
  - `GET v1/matches/{pattern-fragment}[?(spent|unspent)]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches1Ary)
  - `GET v1/matches/{pattern-fragment}/{pattern-fragment}[?(spent|unspent)]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches2Ary)

<br/>

- [📌 #21](https://github.com/CardanoSolutions/kupo/issues/21) New HTTP endpoint to retrieve Plutus' datum pre-image from a datum hash digest. Behind the scene, Kupo now track any datum found in transactions' witnesses set or output (inline datums). Note that, datums that aren't associated to any existing pattern matches are eventually garbage-collected. 
  - `GET v1/datums/{datum-hash}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getDatumByHash)

<br/>

- [📌 #21](https://github.com/CardanoSolutions/kupo/issues/28) New HTTP endpoint to retrieve native & Plutus' script pre-image from a script hash digest. Behind the scene, Kupo now track any script found in transactions' witnesses set, auxiliary data and/or outputs (reference scripts).
  - `GET v1/scripts/{script-hash}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getScriptByHash)

<br/>

- [📌 #40](https://github.com/CardanoSolutions/kupo/issues/40) New HTTP endpoint to retrieve patterns that _includes_ a given pattern. Useful to check if an address is matched by a given configuration. 
  - `GET v1/patterns/{pattern-fragment}[/{pattern-fragment}]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/matchPattern1Ary)

<br/>

- [📌 #40](https://github.com/CardanoSolutions/kupo/issues/21) New optional command-line option `--gc-interval` to tweak the interval between database's garbage collection. Each garbage collection takes a bit of time (few seconds) and pauses the indexer while doing so; A too short interval may have a strong impact on overall performance. A too long interval may greatly
increase the time needed for collecting garbage. Optimal value depends on your use-case, but the default (3 minute) is seemingly a sensible default.

<br/>

- [📌 #24](https://github.com/CardanoSolutions/kupo/issues/24) - New HTTP endpoint to retrieve a point on-chain from a given slot. The endpoint is flexible and allows for retrieving the ancestor 
  of a known point very easily. This is handy in combination with other protocols that leverage on-chain points and intersections (like [Ogmios' chain-sync](https://ogmios.dev/mini-protocols/local-chain-sync/)). 

  - `GET v1/checkpoints/{slot-no}[?strict]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getCheckpointBySlot)

<br/>

- [📌 #35](https://github.com/CardanoSolutions/kupo/issues/35) - New HTTP header `X-Most-Recent-Checkpoint` to every (successful) response. It contains the slot number of the current database most recent checkpoint. This allows client to know which slot a certain query is accurate of. 

#### Changed

- [📌 #17](https://github.com/CardanoSolutions/kupo/issues/20) - The `slot_no` and `header_hash` fields are no longer accessible on top-level match result objects. Instead, they're now nested under a `created_at` field, analogous to how `spent_at` has been introduced. 

- [📌 #24](https://github.com/CardanoSolutions/kupo/issues/24) - Fixed a bug where listing checkpoints would sometimes return duplicate entries. 

- [📌 #39](https://github.com/CardanoSolutions/kupo/issues/39) - Inserting a new pattern (i.e. `PUT v1/patterns/{pattern-fragment}`) now requires to provide a rollback point, to which the server will rollback and start synchronizing again. The old behavior can be recovered by simply passing the most recent checkpoint as a rollback point. Note that, you may add an already existing pattern if you only need, for some reason, to rollback the indexer to some previous point in time. See the [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPattern1Ary) for details.

- Fixed a bug where the server would systematically reject any request to dynamically remove a pattern (because deemed overlapping with existing patterns). 

#### Removed

- N/A

### [1.0.1] - 2022-05-04

#### Added

N/A

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
